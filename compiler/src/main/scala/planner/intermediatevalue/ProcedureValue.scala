package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.PolymorphicSignature
import llambda.compiler.planner.{InvokableProc, PlanWriter}

import llambda.compiler.planner.{step => ps}
import llambda.compiler.valuetype.{polymorphic => pm}


trait ProcedureValue extends IntermediateValue {
  /** Returns the polymorphic signature of the invokable procedure */
  protected val polySignature: PolymorphicSignature

  /** Loads the entry point in the passed plan and returns its TempValue */
  protected def planEntryPoint()(implicit plan: PlanWriter): ps.TempValue

  /** Loads or create a ProcedureCell referencing the invokable procedure in the passed plan */
  protected def planSelf()(implicit plan: PlanWriter): ps.TempValue

  lazy val polyProcedureType: pm.PolymorphicProcedureType = polySignature.toPolymorphicProcedureType
  lazy val hasSelfArg: Boolean = polySignature.template.hasSelfArg

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

  def planInvokableProc()(implicit plan: PlanWriter): InvokableProc = {
    val selfTempOpt = if (polySignature.upperBound.hasSelfArg) {
      Some(planSelf())
    }
    else {
      None
    }

    InvokableProc(polySignature.upperBound, planEntryPoint(), selfTempOpt)
  }
}
