package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}

object MergeIdenticalSteps extends FunctionConniver {
  /** Mapping of step merge keys to the result value of that step */
  type AvailableMerges = Map[Any, ps.TempValue]

  protected def dropAndRename(steps: List[ps.Step], renames: Map[ps.TempValue, ps.TempValue], availableMerges: AvailableMerges, acc: List[ps.Step]): (List[ps.Step], Map[ps.TempValue, ps.TempValue]) = steps match {
    case step :: tailSteps =>
      val renamedStep = step.renamed { tempValue =>
        renames.getOrElse(tempValue, tempValue)
      }

      renamedStep match {
        case condBranch: ps.CondBranch =>
          val mappedStep = condBranch.mapInnerBranches { case (innerSteps, innerResultTemps) =>
            val (mappedSteps, finalRenames) = dropAndRename(innerSteps, renames, availableMerges, Nil)

            // Map the result values using the branch step's renames in case it came from an inner dropped cast
            val mappedResultTemps = innerResultTemps.map { resultTemp =>
              finalRenames.getOrElse(resultTemp, resultTemp)
            }

            (mappedSteps, mappedResultTemps)
          }

          dropAndRename(tailSteps, renames, availableMerges, mappedStep :: acc)

        case mergeableStep: ps.MergeableStep =>
          // Rename the result temp for comparison purposes
          val mergeKey = mergeableStep.mergeKey

          // Available merge for this?
          availableMerges.get(mergeKey) match {
            case Some(existingTemp) =>
              // We can replace this mergable step with a rename
              val newRenames = renames + (mergeableStep.result -> existingTemp)
              dropAndRename(tailSteps, newRenames, availableMerges, acc)

            case None =>
              // We have a new conversion available
              val newAvailableMerges =  availableMerges + (mergeKey -> mergeableStep.result)
              dropAndRename(tailSteps, renames, newAvailableMerges, renamedStep :: acc)
          }

        case otherStep =>
          dropAndRename(tailSteps, renames, availableMerges, otherStep :: acc)
      }

    case Nil =>
      (acc.reverse, renames)
  }

  def mergeSteps(steps: List[ps.Step]): List[ps.Step] =
    dropAndRename(steps, Map(), Map(), Nil)._1

  protected def conniveFunction(function: PlannedFunction): PlannedFunction = {
    function.copy(
      steps=mergeSteps(function.steps)
    )
  }
}
