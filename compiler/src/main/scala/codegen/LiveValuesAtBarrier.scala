package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.ProcedureAttribute

object LiveValuesAtBarrier {
  object Result {
    sealed trait Result
    case class BarrierEncountered(liveValues : Set[ps.TempValue]) extends Result
    case object NoBarrier extends Result
  }

  def apply(steps : List[ps.Step], initialValues : Set[ps.TempValue]) : Result.Result = {
    import Result._

    steps match {
      case ps.DisposeValues(toDispose) :: tail =>
        LiveValuesAtBarrier(tail, initialValues -- toDispose)

      case (nestingStep : ps.NestingStep) :: tail =>
        val nestedResults = nestingStep.innerBranches.map { case (nestedSteps, _) =>
          LiveValuesAtBarrier(nestedSteps, initialValues)
        }

        // Determine which values will be live in every branch at the barrier - these will be rooted by the flush at
        // the beginning of the branch
        nestedResults.reduce { (leftResult : Result, rightResult : Result) =>
          (leftResult, rightResult) match {
            case (BarrierEncountered(leftValues), BarrierEncountered(rightValues)) =>
              BarrierEncountered(leftValues & rightValues)

            case _ =>
              NoBarrier
          }
        } match {
          case barrier : BarrierEncountered =>
            // All branches have a barrier, stop here
            barrier

          case NoBarrier =>
            // Continue to look for barriers in the common tail after the branch
            LiveValuesAtBarrier(tail, initialValues)
        }

      case (_ : ps.Return | _ : ps.TailCall) :: _ =>
        // These terminate - no need to root anything past this
        NoBarrier

      case (invokeStep : ps.Invoke) :: _ if invokeStep.signature.attributes.contains(ProcedureAttribute.NoReturn) =>
        // These throw exceptions or exit
        NoBarrier

      case (inputDisposable : ps.InputDisposableStep) :: tail =>
        val disposedValues = initialValues -- inputDisposable.inputToDispose

        if (inputDisposable.canAllocate) {
          BarrierEncountered(disposedValues)
        }
        else {
          LiveValuesAtBarrier(tail, disposedValues)
        }

      case barrierStep :: tail if barrierStep.canAllocate =>
        BarrierEncountered(initialValues)

      case _ :: tail =>
        LiveValuesAtBarrier(tail, initialValues)

      case Nil =>
        // Never encountered a GC barrier
        NoBarrier
    }
  }
}
