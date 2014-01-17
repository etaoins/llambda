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
      // Remove the argValues or else we'll "helpfully" try to dispose at
      // them at the top of the branches
      val newTrueSteps = disposeTestOption.toList ++
        discardUnusedValues(Set(), trueSteps.reverse, usedValues + trueValue).reverse

      val newFalseSteps = disposeTestOption.toList ++
        discardUnusedValues(Set(), falseSteps.reverse, usedValues + falseValue).reverse

      val newUsedValues = condBranch.inputValues ++ usedValues 

      // NB this is is reverse order
      disposeResultOption.toList ++
        List(ps.CondBranch(result, test, newTrueSteps, trueValue, newFalseSteps, falseValue)) ++
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
      Set()
    ).reverse

    function.copy(steps=newSteps)
  }
}
