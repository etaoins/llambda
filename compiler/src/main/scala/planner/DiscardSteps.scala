package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

/** Discard steps with unused results */
object DiscardSteps {
  private def innerOutputValuesForStep(step: ps.Step): Set[ps.TempValue] = step match {
    case condBranch: ps.CondBranch =>
      condBranch.innerBranches.flatMap(_._1).flatMap(innerOutputValuesForStep).toSet

    case other =>
      other.outputValues
  }

  /** Iterates over a branch in reverse order discarding values on their last use
    *
    * @param  reverseSteps  Branch steps in reverse order
    * @param  usedValues    Values used at this point in the branch. This set grows as the branch is scanned in reverse
    *                       order
    * @param  acc           Result accumulator for tail call optimisation
    * @return  List of new branch steps in forward order
    */
  private def discardUnusedSteps(
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

      // Recurse down the branches
      val newStep = dephiedCondBranch.mapInnerBranches { (branchSteps, outputValues) =>
        val nestedUsedValues = usedValues ++ outputValues

        (discardUnusedSteps(branchSteps.reverse, nestedUsedValues, Nil), outputValues)
      }

      newStep match {
        case ps.CondBranch(_, trueSteps, falseSteps, Nil) if trueSteps == falseSteps =>
          // This CondBranch can be simplified away. Use the original steps from an arbitrary branch.
          discardUnusedSteps(condBranch.trueSteps.reverse ++ reverseTail, usedValues, acc)

        case _ =>
          // Output values created by steps within the CondBranch
          val innerOutputValues = innerOutputValuesForStep(dephiedCondBranch)
          // Input values that are not created within the CondBranch
          val externalInputValues = dephiedCondBranch.inputValues -- innerOutputValues

          val newUsedValues = externalInputValues ++ usedValues

          val newAcc = newStep :: acc
          discardUnusedSteps(reverseTail, newUsedValues, newAcc)
      }

    case (loadRecordFieldsStep: ps.LoadRecordLikeFields) :: reverseTail =>
      val usedFieldsToLoad = loadRecordFieldsStep.fieldsToLoad.filter { case (field, tempValue) =>
        usedValues.contains(tempValue)
      }

      if (usedFieldsToLoad.isEmpty) {
        // Discard completely
        discardUnusedSteps(reverseTail, usedValues, acc)
      }
      else {
        val newStep = loadRecordFieldsStep
          .copy(fieldsToLoad=usedFieldsToLoad)
          .assignLocationFrom(loadRecordFieldsStep)

        val newUsedValues = usedValues ++ newStep.inputValues

        val newAcc = newStep :: acc
        discardUnusedSteps(reverseTail, newUsedValues, newAcc)
      }

    case discardableStep :: reverseTail
        if discardableStep.discardable && (usedValues & discardableStep.outputValues).isEmpty =>
      // We can drop this step completely
      discardUnusedSteps(reverseTail, usedValues, acc)

    case otherStep :: reverseTail =>
      val newUsedValues = usedValues ++ otherStep.inputValues

      val newAcc = otherStep :: acc
      discardUnusedSteps(reverseTail, newUsedValues, newAcc)

    case Nil =>
      // We've reached the top of the branch
      acc
  }

  def apply(steps: List[ps.Step]): List[ps.Step] =
    discardUnusedSteps(steps.reverse, Set(), Nil)
}
