package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.planner._

/** Connivers are optimisers working on planned programs
  *
  * They mutate and re-organize planned steps and return the new planner functions. They are intended to perform low
  * level optimisations on plans across expression boundaries
  */
abstract class Conniver extends Function1[Map[String, PlannedFunction], Map[String, PlannedFunction]]

/** Function connivers are connivers that work at a per-function level
  *
  * This is a helper class for a common conniver pattern
  */
abstract class FunctionConniver extends Conniver {
  protected def conniveFunction(function : PlannedFunction) : PlannedFunction

  def apply(plannedFunctions : Map[String, PlannedFunction]) : Map[String, PlannedFunction] =
    plannedFunctions.mapValues(conniveFunction)
}
