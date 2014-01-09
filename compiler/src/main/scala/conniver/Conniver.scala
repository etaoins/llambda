package io.llambda.compiler.conniver

import io.llambda.compiler.planner._
import io.llambda.compiler.planner.{step => ps}

/** Connivers are optimizers working on planned programs
  *
  * They mutate and re-organize planned steps and return the new planned
  * functions. They are intended to work at a lower level than optimizers
  * by working across planned expression boundaries to remove redundancies.
  */
abstract class Conniver extends Function1[Map[String, PlannedFunction], Map[String, PlannedFunction]]

/** Function connivers are connivers that work at a per-function level
  *
  * This is a helper class for a common conniver pattern
  */
abstract class FunctionConniver extends Conniver {
  protected def conniveFunction(steps : List[ps.Step]) : List[ps.Step] 
  
  def apply(plannedFunctions : Map[String, PlannedFunction]) : Map[String, PlannedFunction] =
    plannedFunctions.mapValues { plannedFunction =>
      val mergedSteps = conniveFunction(plannedFunction.steps)
      plannedFunction.copy(steps=mergedSteps)
    }
}
