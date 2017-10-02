package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{InvokableProc, PlanWriter}

import llambda.compiler.planner.{step => ps}
import llambda.compiler.valuetype.{polymorphic => pm}


trait ProcedureValue extends IntermediateValue {
  val polyProcedureType: pm.PolymorphicProcedureType
  val hasSelfArg: Boolean

  /** Optionally returns the native symbol for the procedure
    *
    * This is used for generating error messages. For some procedure values this will be unknown.
    */
  def nativeSymbolOpt(implicit plan: PlanWriter): Option[String]

  /** Creates a copy of this invokable procedure with a new self temp */
  def withSelfTemp(tempValue: ps.TempValue): ProcedureValue

  /** Indicates if this procedure has side effects
    *
    * If not then unused invokations of this procedure may be discarded
    */
  def hasSideEffects(arity: Int): Boolean =
    true

  def planInvokableProc()(implicit plan: PlanWriter): InvokableProc
}
