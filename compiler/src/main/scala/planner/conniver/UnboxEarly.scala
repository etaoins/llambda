package io.llambda.compiler.planner.conniver
import io.llambda

import scala.collection.immutable.ListMap

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}

/** Moves any unboxing steps to after the last use of the input boxed value
  *
  * This reduces GC overhead by allowing the GC managed boxed value to be replaced with its unboxed equivalent as soon
  * as possible. This requires less values to be GC rooted and saved/restored across GC barriers.
  */
object UnboxEarly extends FunctionConniver {
  protected def moveUnboxingSteps(
      reverseSteps: List[ps.Step],
      movingSteps: ListMap[ps.TempValue, ps.UnboxValue],
      acc: List[ps.Step]
  ): List[ps.Step] = reverseSteps match {
    case (unboxing: ps.UnboxValue) :: reverseTail =>
      // Remove this from the list of steps and place it inside movingSteps
      val newMovingSteps = movingSteps + (unboxing.boxed -> unboxing)
      moveUnboxingSteps(reverseTail, newMovingSteps, acc)

    case other :: reverseTail =>
      // If this step uses the boxed value we shouldn't move the unboxing earlier - this would require us to keep both
      // the boxed and unboxed version live which is less efficient than just keeping the boxed version
      // If this step produces the boxed value we cannot move the unboxing before it
      val allValues = other.inputValues ++ other.outputValues

      // Determine which steps need to be placed after this step
      val (movingStepsToPlace, remainingMovingSteps) = movingSteps.partition { case (boxedValue, _) =>
        allValues.contains(boxedValue)
      }

      // Do we need to recurse down this step?
      val recursedStep = other match {
        case nestingStep: ps.NestingStep =>
          nestingStep.mapInnerBranches { case (innerSteps, innerResultTemp) =>
            val newSteps = moveUnboxingSteps(innerSteps.reverse, ListMap(), Nil)
            (newSteps, innerResultTemp)
          }

        case _ =>
          other
      }

      // Place the unboxing steps in their original order
      val unboxingSteps = movingStepsToPlace.map(_._2).toList.reverse

      moveUnboxingSteps(reverseTail, remainingMovingSteps, recursedStep :: (unboxingSteps ++ acc))

    case Nil =>
      // Place all unclaimed unboxing steps
      movingSteps.values.toList.reverse ++ acc
  }

  protected[conniver] def conniveSteps(steps: List[ps.Step]): List[ps.Step] =
    moveUnboxingSteps(steps.reverse, ListMap(), Nil)

  protected def conniveFunction(function: PlannedFunction): PlannedFunction = {
    function.copy(
      steps=conniveSteps(function.steps)
    )
  }
}
