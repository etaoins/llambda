package io.llambda.compiler.conniver
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}

object MergeIdenticalSteps extends FunctionConniver {
  /** Mapping of step merge keys to the result value of that step */
  type AvailableMerges = Map[Any, ps.TempValue]

  // This is used when comparing mergeable steps
  // They're equal for our purposes regardless if their result is equal
  private object PlaceholderResultTemp extends ps.TempValue(false)

  protected def dropAndRename(steps : List[ps.Step], renames : Map[ps.TempValue, ps.TempValue], availableMerges : AvailableMerges, acc : List[ps.Step]) : (List[ps.Step], Map[ps.TempValue, ps.TempValue]) = steps match {
    case step :: tailSteps =>
      val renamedStep = step.renamed { tempValue =>
        renames.getOrElse(tempValue, tempValue)
      }

      renamedStep match {
        case nestingStep : ps.NestingStep =>
          val mappedStep = nestingStep.mapInnerBranches { case (innerSteps, innerResultTemp) =>
            val (mappedSteps, finalRenames) = dropAndRename(innerSteps, renames, availableMerges, Nil)

            // Map the result value using the branch step's renames in case it came from an inner dropped cast
            val mappedResultTemp = finalRenames.getOrElse(innerResultTemp, innerResultTemp)

            (mappedSteps, mappedResultTemp)
          }

          dropAndRename(tailSteps, renames, availableMerges, mappedStep :: acc)

        case mergeableStep : ps.MergeableStep =>
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

  protected def conniveFunction(function : PlannedFunction) : PlannedFunction = {
    function.copy(
      steps=dropAndRename(function.steps, Map(), Map(), Nil)._1
    )
  }
}
