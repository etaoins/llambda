package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException

object InferArgumentTypes {
  private case class RetypingResult(
    replaceArgTempValue : ps.TempValue,
    replaceArgType : vt.ValueType,
    steps : List[ps.Step]
  )

  private def stepCanTerminate(step : ps.Step) : Boolean = step match {
    case nestingStep : ps.NestingStep =>
      nestingStep.innerBranches.exists { branch =>
        branch._1.exists(stepCanTerminate(_))
      }

    case _ : ps.Return =>
      // This the only actual terminating instruction
      true 

    case _ =>
      false
  }

  private def retypeArgument(argValue : ps.TempValue, steps : List[ps.Step]) : RetypingResult = {
    def abortRetyping =
      RetypingResult(
        replaceArgTempValue=argValue,
        replaceArgType=vt.IntrinsicCellType(ct.DatumCell),
        steps=steps
      )

    steps match {
      case ps.CastCellToSubtypeChecked(result, _, value, toType, _) :: tailSteps if value == argValue =>
        // we found an unconditional checked subtype cast!

        // It's valid for future steps to use the original <datum-cell> value
        // Casting to the supertype is free so make a <datum-cell> available under
        // the original TempValue
        val supercastStep = ps.CastCellToTypeUnchecked(value, result, ct.DatumCell)

        RetypingResult(
          replaceArgTempValue=result,
          replaceArgType=vt.IntrinsicCellType(toType),
          steps=supercastStep :: tailSteps 
        )

      case (nestingStep : ps.NestingStep) :: tailSteps if stepCanTerminate(nestingStep) =>
        // If one side of the branch terminates then we can't be sure the cast
        // to subtype will be unconditionally executed.

        // If the nesting step in an exception control structure all some of the
        // steps may not be executed even if they're only one branch

        abortRetyping

      case userStep :: tailSteps if userStep.inputValues.contains(argValue) =>
        // We hit a user before we found an unchecked cast
        abortRetyping

      case nonUserStep :: tailSteps =>
        // Keep looking for a cast
        val innerResult = retypeArgument(argValue, tailSteps)

        innerResult.copy(steps=nonUserStep :: innerResult.steps)

      case Nil =>
        abortRetyping
    }
  }

  def apply(initialFunction : PlannedFunction) : PlannedFunction = {
    val worldPtrProcessedFunction = if (initialFunction.signature.hasWorldArg) {
      // Find the world ptr temp value
      val worldPtrTemp = initialFunction.namedArguments.head._2
      val worldPtrUsed = initialFunction.steps.exists(_.inputValues.contains(worldPtrTemp))

      if (worldPtrUsed) {
        initialFunction
      }
      else {
        initialFunction.copy(
          signature=initialFunction.signature.copy(
            hasWorldArg=false
          ),
          namedArguments=initialFunction.namedArguments.tail
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
      
      if (signature.fixedArgs(fixedArgIndex) == vt.IntrinsicCellType(ct.DatumCell)) {
        // Try to rewrite this to something more specific
        val (argName, argValue) = function.namedArguments(namedArgIndex)

        val result = retypeArgument(argValue, function.steps)

        val newSignature = signature.copy(
          fixedArgs=signature.fixedArgs.updated(fixedArgIndex, result.replaceArgType)
        )

        PlannedFunction(
          signature=newSignature,
          namedArguments=function.namedArguments.updated(namedArgIndex, (argName, result.replaceArgTempValue)),
          steps=result.steps
        )
      }
      else {
        // Skip this arg
        function
      }
    }
  }
}
