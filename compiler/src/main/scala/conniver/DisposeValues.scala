package io.llambda.compiler.conniver
import io.llambda

import io.llambda.compiler.planner._
import io.llambda.compiler.planner.{step => ps}

/** Disposes values after their last use
  *
  * This reduces both the computational and memory overhead of garbage 
  * collection by discarding values. It has no effect on generated code for
  * non-GC managed values.
  */
object DisposeValues extends FunctionConniver {
  private def discardUnusedValues(argValues : Set[ps.TempValue], reverseSteps : List[ps.Step], usedValues : Set[ps.TempValue]) : List[ps.Step] = reverseSteps match {
    case (nestingStep : ps.NestingStep) :: reverseTail =>
      // Determine which input values are no longer used
      val unusedInputValues = nestingStep.outerInputValues.filter(!usedValues.contains(_))

      // Step to dispose the result outputs if they're unused
      // This will be placed after the step itself
      val unusedOutputValues = nestingStep.outputValues.filter(!usedValues.contains(_))
      val disposeOutputSteps = unusedOutputValues.toList.map { unusedValue =>
        ps.DisposeValue(unusedValue)
      }

      // Recurse down the branches
      val newStep = nestingStep.mapInnerBranches { (branchSteps, outputValue) =>
        // Pass the unused input values as argument values
        // If they're not used within the branch they'll be disposed at the top of it
        discardUnusedValues(unusedInputValues, branchSteps.reverse, usedValues + outputValue).reverse
      }

      val newUsedValues = nestingStep.inputValues ++ usedValues 

      // NB this is is reverse order
      disposeOutputSteps ++
        List(newStep) ++
        discardUnusedValues(argValues, reverseTail, newUsedValues)

    case (invoke @ ps.Invoke(resultOption, signature, entryPoint, arguments)) :: reverseTail =>
      val disposeResultOption = resultOption match { 
        case Some(result) if !usedValues.contains(result) =>
          Some(ps.DisposeValue(result))

        case _ =>
          None
      }

      val newArguments = arguments.map { argument =>
        // Arguments have be disposed inside the invoke
        // If they were disposed before there would be no way to reference them
        val shouldDispose = !usedValues.contains(argument.tempValue)

        argument.copy(dispose=shouldDispose)
      }

      val newUsedValues = invoke.inputValues ++ usedValues

      // NB this is in reverse order
      disposeResultOption.toList ++
        List(ps.Invoke(resultOption, signature, entryPoint, newArguments)) ++
        discardUnusedValues(argValues, reverseTail, newUsedValues) 

    case nonBranching :: reverseTail =>
      // If this is the last use of any of the input or output values they
      // should be discarded
      val allStepValues = nonBranching.inputValues ++ nonBranching.outputValues

      val disposeList = (allStepValues -- usedValues).toList.map { value =>
        ps.DisposeValue(value)
      }

      // All the input values are now used
      val newUsedValues = usedValues ++ nonBranching.inputValues

      // NB this is reverse order
      disposeList ++ List(nonBranching) ++ discardUnusedValues(argValues, reverseTail, newUsedValues)

    case Nil =>
      // Dispose all unused args
      // In conditional branches argValues will be empty
      val disposeList = (argValues -- usedValues).toList.map { value =>
        ps.DisposeValue(value) 
      }

      disposeList
  }

  def conniveFunction(function : PlannedFunction) : PlannedFunction = {
    val argValues = function.namedArguments.map(_._2).toSet

    val newSteps = discardUnusedValues(
      argValues,
      function.steps.reverse, 
      // Nothing directly uses the world ptr until PlanCellAllocations runs
      // Artifically set it as used - it's not GC managed so there's no real
      // gain in disposing it. 
      function.worldPtrOption.toSet
    ).reverse

    function.copy(steps=newSteps)
  }
}
