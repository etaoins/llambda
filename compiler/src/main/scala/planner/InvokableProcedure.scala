package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.PolymorphicSignature
import llambda.compiler.planner.{step => ps}

trait InvokableProcedure {
  /** Returns the polymorphic signature of the invokable procedure */
  val polySignature : PolymorphicSignature

  /** Loads the entry point in the passed plan and returns its TempValue */
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue

  /** Loads or create a ProcedureCell referencing the invokable procedure in the passed plan */
  def planSelf()(implicit plan : PlanWriter) : ps.TempValue

  /** Optionally returns the native symbol for the procedure
    *
    * This is used for generating error messages. For some procedure values this will be unknown.
    */
  def nativeSymbolOpt(implicit plan : PlanWriter) : Option[String]

  /** Creates a copy of this invokable procedure with a new self temp */
  def withSelfTemp(tempValue : ps.TempValue) : InvokableProcedure

  /** Indicates if this procedure has side effects
    *
    * If not then unused invokations of this procedure may be discarded
    */
  def hasSideEffects(arity : Int) : Boolean =
    true
}
