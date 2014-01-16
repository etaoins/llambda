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
  private def discardUnusedValues(reverseSteps : List[ps.Step], usedValues : Set[ps.TempValue]) : List[ps.Step] = reverseSteps match {
    case (condBranch @ ps.CondBranch(result, test, trueSteps, trueValue, falseSteps, falseValue)) :: reverseTail =>
      // Step to dispose the test if it's no longer used
      // This will be placed at the beginning of both branches
      val disposeTestOption = if (!usedValues.contains(test)) {
        Some(ps.DisposeValue(test))
      }
      else {
        None
      }

      // Step to dispose the result if it's unused
      // This will be placed after the CondBranch
      val disposeResultOption = if (!usedValues.contains(result)) {
        Some(ps.DisposeValue(result))
      }
      else {
        None
      }

      // Recurse down the branches
      val newTrueSteps = disposeTestOption.toList ++ discardUnusedValues(trueSteps.reverse, usedValues + trueValue).reverse
      val newFalseSteps = disposeTestOption.toList ++ discardUnusedValues(falseSteps.reverse, usedValues + falseValue).reverse

      val newUsedValues = condBranch.inputValues ++ usedValues 

      // NB this is is reverse order
      disposeResultOption.toList ++
        List(ps.CondBranch(result, test, newTrueSteps, trueValue, newFalseSteps, falseValue)) ++
        discardUnusedValues(reverseTail, newUsedValues)

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
        discardUnusedValues(reverseTail, newUsedValues) 

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
      disposeList ++ List(nonBranching) ++ discardUnusedValues(reverseTail, newUsedValues)

    case Nil =>
      // XXX: We should probably discard arguments
      Nil
  }

  def conniveFunction(steps : List[ps.Step]) : List[ps.Step] = {
    discardUnusedValues(steps.reverse, Set[ps.TempValue]()).reverse
  }
}
