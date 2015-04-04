package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.ProcedureAttribute

object LiveValuesAtBarrier {
  object Result {
    sealed trait Result

    /** GC barrier was encountered in all execution paths
      *
      * @param  liveValues  Set of values live at the GC barrier
      */
    case class BarrierEncountered(liveValues : Set[ps.TempValue]) extends Result

    /** No GC barrier was encountered in at least one execution path */
    case object NoBarrier extends Result
  }

  /** Determines which TempValues will definitely cross a GC barrier in the passed steps
    *
    * This only considers values passed in from initialValues. Values created during the execution of the passed steps
    * will not be included in the result.
    *
    * When encountering with branches this will follow each branch and only consider values that crossed a GC barrier
    * in both branches. Terminating step are not considered GC barriers as the current function's GC state is cleaned
    * up before the step is executed.
    *
    * @param  steps           List of steps in forward order to examine for GC barriers
    * @param  initialValues   Set of values to consider live at the beginning of the steps
    */
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

      case terminating :: _ if terminating.alwaysTerminates =>
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
