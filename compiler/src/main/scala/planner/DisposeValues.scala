package io.llambda.compiler.planner
import io.llambda

import io.llambda.compiler.planner.{step => ps}

/** Disposes values after their last use
  *
  * This reduces both the computational and memory overhead of garbage collection by discarding values. It has no effect
  * on generated code for non-GC managed values.
  */
object DisposeValues {
  /**  Iterates over a branch in reverse order discarding values on their last use
    *
    * @param  branchInputValues  Input values to the branch. For a procedure these are the procedure's arguments.
    *                            For conditional branches these will be the values in the test. These are discarded at
    *                            the top of the branch if they're unused.
    * @param  branchSteps        Branch steps in reverse order
    * @param  usedValues         Values used at this point in the branch. This set grows as the branch is scanned in
    *                            reverse order
    * @param  acc                Result accumulator for tail call optmization
    * @return  List of new branch steps in forward order
    */
  private def discardUnusedValues(branchInputValues : Set[ps.TempValue], reverseSteps : List[ps.Step], usedValues : Set[ps.TempValue], acc : List[ps.Step]) : List[ps.Step] = reverseSteps match {
    case (condStep : ps.CondBranch) :: reverseTail =>
      // Determine which input values are no longer used
      val unusedInputValues = condStep.outerInputValues.filter(!usedValues.contains(_))

      // Step to dispose the result outputs if they're unused
      // This will be placed after the step itself
      val unusedOutputValues = condStep.outputValues.filter(!usedValues.contains(_))
      val disposeOutputSteps = unusedOutputValues.toList.map { unusedValue =>
        ps.DisposeValue(unusedValue)
      }

      // Recurse down the branches
      val newStep = condStep.mapInnerBranches { (branchSteps, outputValue) =>
        // Pass the unused input values as argument values
        // If they're not used within the branch they'll be disposed at the top of it
        (discardUnusedValues(unusedInputValues, branchSteps.reverse, usedValues + outputValue, Nil), outputValue)
      }

      val newUsedValues = condStep.inputValues ++ usedValues 

      val newAcc = newStep :: (disposeOutputSteps ++ acc)
      discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc)

    case (invoke @ ps.Invoke(resultOption, signature, entryPoint, arguments, tailCall)) :: reverseTail =>
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
      val newInvoke = ps.Invoke(resultOption, signature, entryPoint, newArguments, tailCall).assignLocationFrom(invoke)

      val newAcc = newInvoke :: (disposeResultOption.toList ++ acc)
      discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc) 

    case (disposableStep : ps.NullipotentStep) :: reverseTail if !usedValues.contains(disposableStep.result) =>
      // We can drop this step completely
      discardUnusedValues(branchInputValues, reverseTail, usedValues, acc)

    case nonBranching :: reverseTail =>
      // If this is the last use of any of the input or output values they should be discarded
      val allStepValues = nonBranching.inputValues ++ nonBranching.outputValues

      val disposeList = (allStepValues -- usedValues).toList.map { value =>
        ps.DisposeValue(value)
      }

      // All the input values are now used
      val newUsedValues = usedValues ++ nonBranching.inputValues

      val newAcc = nonBranching :: (disposeList ++ acc)
      discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc)

    case Nil =>
      // We've reached the top of the branch
      // Dispose all unused branch input values
      val disposeList = (branchInputValues -- usedValues).toList.map { value =>
        ps.DisposeValue(value) 
      }

      disposeList ++ acc
  }

  def apply(function : PlannedFunction) : PlannedFunction = {
    val branchInputValues = function.namedArguments.map(_._2).toSet

    val newSteps = discardUnusedValues(
      branchInputValues,
      function.steps.reverse, 
      // Nothing directly uses the world ptr until PlanCellAllocations runs
      // Artifically set it as used - it's not GC managed so there's no real gain in disposing it. 
      function.worldPtrOpt.toSet,
      Nil
    )

    function.copy(steps=newSteps)
  }
}
