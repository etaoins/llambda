package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.planner.{step => ps}


object PlanRuntimeAssert {
  /** Plans a runtime assertion that a predicate is true
    *
    * @param  predicate     TempValue of type i1
    * @param  errorMessage  Error to raise if the predicate is false
    * @param  evidenceOpt   Optional error evidence as a pointer of type AnyCell
    */
  def apply(
    predicate: ps.TempValue,
    errorMessage: RuntimeErrorMessage,
    evidenceOpt: Option[ps.TempValue] = None
  )(implicit plan: PlanWriter): Unit = {
    val signalErrorStep = ps.SignalError(errorMessage, evidenceOpt)
    signalErrorStep.assignLocationFrom(plan.activeContextLocated)

    plan.steps += ps.CondBranch(predicate, Nil, List(signalErrorStep), Nil)
  }
}
