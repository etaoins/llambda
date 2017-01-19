package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

/** Disposes values after their last use
  *
  * This reduces both the computational and memory overhead of garbage collection by discarding values. It has no effect
  * on generated code for non-GC managed values.
  */
object DisposeValues {
  private def disposeValuesToSteps(toDispose: Set[ps.TempValue]): List[ps.Step] = if (toDispose.isEmpty) {
    Nil
  }
  else {
    List(ps.DisposeValues(toDispose))
  }

  def innerOutputValuesForStep(step: ps.Step): Set[ps.TempValue] = step match {
    case condBranch: ps.CondBranch =>
      condBranch.innerBranches.flatMap(_._1).flatMap(innerOutputValuesForStep).toSet

    case other =>
      other.outputValues
  }

  /** Iterates over a branch in reverse order discarding values on their last use
    *
    * @param  branchInputValues  Input values to the branch. For a procedure these are the procedure's arguments.
    *                            For conditional branches these will be the values in the test. These are discarded at
    *                            the top of the branch if they're unused.
    * @param  reverseSteps       Branch steps in reverse order
    * @param  usedValues         Values used at this point in the branch. This set grows as the branch is scanned in
    *                            reverse order
    * @param  acc                Result accumulator for tail call optmization
    * @return  List of new branch steps in forward order
    */
  private def discardUnusedValues(
      branchInputValues: Set[ps.TempValue],
      reverseSteps: List[ps.Step],
      usedValues: Set[ps.TempValue],
      acc: List[ps.Step]
  ): List[ps.Step] = reverseSteps match {
    case (condBranch: ps.CondBranch) :: reverseTail =>
      // Remove any value phis where the result is unused
      val usedValuePhis = condBranch.valuePhis.filter { valuePhi =>
        usedValues.contains(valuePhi.result)
      }

      val dephiedCondBranch = condBranch.copy(valuePhis=usedValuePhis).assignLocationFrom(condBranch)

      // Build a set of output values generated inside the branch. This is used to distinguish input values that come
      // from before the branch (and therefore need to be disposed) versus input values that come from within the branch
      val innerOutputValues = innerOutputValuesForStep(dephiedCondBranch)

      // Determine which input values come from outside the branch
      val externalInputValues = dephiedCondBranch.inputValues -- innerOutputValues
      val unusedExternalInputValues = externalInputValues.filter(!usedValues.contains(_))

      // Recurse down the branches
      val newStep = dephiedCondBranch.mapInnerBranches { (branchSteps, outputValues) =>
        // Pass the unused input values as argument values
        // If they're not used within the branch they'll be disposed at the top of it
        val nestedInputValues = unusedExternalInputValues
        val nestedUsedValues = usedValues ++ outputValues

        (discardUnusedValues(nestedInputValues, branchSteps.reverse, nestedUsedValues, Nil), outputValues)
      }

      newStep match {
        case ps.CondBranch(_, trueSteps, falseSteps, Nil) if trueSteps == falseSteps =>
          // This CondBranch can be simplified away. Use the original steps from an arbitrary branch so we don't need to
          // remove the DisposeValues steps.
          //
          // This is arguably out-of-scope for the DisposeValues pass, but:
          // - A CondBranch may only become identical after disposing values
          // - This allows the branch condition to become unused and possibly disposed
          discardUnusedValues(branchInputValues, condBranch.trueSteps.reverse ++ reverseTail, usedValues, acc)

        case _ =>
          val newUsedValues = externalInputValues ++ usedValues

          // Dispose of any values used by phis that come from outside the branches and are no longer used
          val externalPhiInputValues = usedValuePhis.flatMap { usedPhi =>
            List(usedPhi.trueValue, usedPhi.falseValue)
          }.toSet & externalInputValues

          val disposeList = disposeValuesToSteps(externalPhiInputValues -- usedValues)
          val newAcc = newStep :: (disposeList ++ acc)

          discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc)
      }

    case discardableStep :: reverseTail
        if discardableStep.discardable && (usedValues & discardableStep.outputValues).isEmpty =>
      // We can drop this step completely
      discardUnusedValues(branchInputValues, reverseTail, usedValues, acc)

    case (inputDisposable: ps.InputDisposableStep) :: reverseTail =>
      // If this is the last use of any of the input values they should be discarded as part of the step
      val inputDisposeSet = inputDisposable.inputValues -- usedValues
      // Any unused output values should be discarded as normal
      val outputDisposeSteps = disposeValuesToSteps(inputDisposable.outputValues -- usedValues)

      // All the input values are now used
      val newUsedValues = usedValues ++ inputDisposable.inputValues

      val newAcc = inputDisposable.withDisposedInput(inputDisposeSet) :: (outputDisposeSteps ++ acc)
      discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc)

    case nonBranching :: reverseTail =>
      val allStepValues = nonBranching.inputValues ++ nonBranching.outputValues
      val disposeList = disposeValuesToSteps(allStepValues -- usedValues)
      val newUsedValues = usedValues ++ nonBranching.inputValues

      val newAcc = nonBranching :: (disposeList ++ acc)
      discardUnusedValues(branchInputValues, reverseTail, newUsedValues, newAcc)

    case Nil =>
      // We've reached the top of the branch
      // Dispose all unused branch input values
      val disposeList = disposeValuesToSteps(branchInputValues -- usedValues)

      disposeList ++ acc
  }

  def apply(function: PlannedFunction): PlannedFunction = {
    val branchInputValues = function.namedArguments.map(_._2).toSet

    val newSteps = discardUnusedValues(
      branchInputValues,
      function.steps.reverse,
      // Allocating steps don't directly use the world ptr until PlanHeapAllocations runs
      // Artifically set it as used - it's not GC managed so there's no real gain in disposing it.
      Set(ps.WorldPtrValue),
      Nil
    )

    function.copy(steps=newSteps)
  }
}
