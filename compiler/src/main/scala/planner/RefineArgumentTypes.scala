package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object RefineArgumentTypes {
  private case class RetypingResult(
    replaceArgTempValue : ps.TempValue,
    replaceArgType : vt.ValueType,
    steps : List[ps.Step]
  )

  private def stepCanTerminate(step : ps.Step) : Boolean = step match {
    case condStep : ps.CondBranch =>
      condStep.innerBranches.exists { branch =>
        branch._1.exists(stepCanTerminate(_))
      }

    case invoke : ps.Invoke =>
      // If the procedure takes a world arg it may throw an exception
      invoke.signature.hasWorldArg

    case _ : ps.Return =>
      // This the only actual terminating instruction
      true 

    case _ =>
      false
  }

  private def retypeArgument(argValue : ps.TempValue, originalType : vt.IntrinsicCellType, steps : List[ps.Step]) : RetypingResult = {
    def abortRetyping =
      RetypingResult(
        replaceArgTempValue=argValue,
        replaceArgType=originalType,
        steps=steps
      )

    steps match {
      case ps.CastCellToSubtypeChecked(result, _, value, toType, _, _) :: tailSteps if value == argValue =>
        // we found an unconditional checked subtype cast!

        // It's valid for future steps to use the original value
        // Casting to the supertype is free so make the original type available under the original TempValue
        val supercastStep = ps.CastCellToTypeUnchecked(value, result, originalType.cellType)

        RetypingResult(
          replaceArgTempValue=result,
          replaceArgType=vt.IntrinsicCellType(toType),
          steps=supercastStep :: tailSteps 
        )

      case (condStep : ps.CondBranch) :: tailSteps if stepCanTerminate(condStep) =>
        // If one side of the branch terminates then we can't be sure the cast to subtype will be unconditionally
        // executed.
        abortRetyping

      case userStep :: tailSteps if userStep.inputValues.contains(argValue) =>
        // We hit a user before we found an unchecked cast
        abortRetyping

      case terminatingStep :: tailSteps if stepCanTerminate(terminatingStep) =>
        // We hit a terminating step
        abortRetyping

      case nonUserStep :: tailSteps =>
        // Keep looking for a cast
        val innerResult = retypeArgument(argValue, originalType, tailSteps)

        innerResult.copy(steps=nonUserStep :: innerResult.steps)

      case Nil =>
        abortRetyping
    }
  }

  private def worldPtrUsedByStep(worldPtrTemp : ps.TempValue, step : ps.Step) : Boolean = step match {
    case consumer : ps.CellConsumer =>
      // This sucks - there are no explicit allocation steps until PlanCellAllocations runs which is the last phase of
      // planning. We have to implicitly know CellConsumers will generate steps requiring the world pointer
      true

    case condStep : ps.CondBranch =>
      // Recurse down each side
      condStep.outerInputValues.contains(worldPtrTemp) || 
        condStep.innerBranches.flatMap(_._1).exists({ branchStep =>
          worldPtrUsedByStep(worldPtrTemp, branchStep)
        })

    case otherStep =>
      otherStep.inputValues.contains(worldPtrTemp)
  }

  private def worldPtrUsedByFunction(function : PlannedFunction) : Boolean = {
    // Find the world ptr temp value
    val worldPtrTemp = function.namedArguments.head._2

    function.steps.exists(worldPtrUsedByStep(worldPtrTemp, _))
  }

  def apply(initialFunction : PlannedFunction) : PlannedFunction = {
    val worldPtrProcessedFunction = if (initialFunction.signature.hasWorldArg) {

      if (worldPtrUsedByFunction(initialFunction)) {
        initialFunction
      }
      else {
        initialFunction.copy(
          signature=initialFunction.signature.copy(
            hasWorldArg=false
          ),
          namedArguments=initialFunction.namedArguments.tail,
          worldPtrOpt=None
        )
      }
    }
    else {
      initialFunction
    }

    val processedSig = worldPtrProcessedFunction.signature

    val fixedArgCount = processedSig.fixedArgs.length
    val prefixArgCount = 
      (if (processedSig.hasWorldArg) 1 else 0) +
      (if (processedSig.hasSelfArg) 1 else 0)

    (0 until fixedArgCount).foldLeft(worldPtrProcessedFunction) { case (function, fixedArgIndex) =>
      // The named arguments include world and self
      val namedArgIndex = fixedArgIndex + prefixArgCount
      val signature = function.signature
      val argType = signature.fixedArgs(fixedArgIndex)
      
      argType match {
        case intrinsicCellType : vt.IntrinsicCellType =>
          // Try to rewrite this to something more specific
          val (argName, argValue) = function.namedArguments(namedArgIndex)

          val result = retypeArgument(argValue, intrinsicCellType, function.steps)

          val newSignature = signature.copy(
            fixedArgs=signature.fixedArgs.updated(fixedArgIndex, result.replaceArgType)
          )

          function.copy(
            signature=newSignature,
            namedArguments=function.namedArguments.updated(namedArgIndex, (argName, result.replaceArgTempValue)),
            steps=result.steps,
            worldPtrOpt=function.worldPtrOpt
          )

        case _ =>
          // Skip this arg
          function
      }
    }
  }
}
