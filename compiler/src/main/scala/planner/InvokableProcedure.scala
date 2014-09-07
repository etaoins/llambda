package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.planner.{step => ps}

trait InvokableProcedure {
  /** Returns the signature of the invokable procedure */
  val signature : ProcedureSignature

  /** Loads the entry point in the passed plan and returns its TempValue */
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue

  /** Loads or create a ProcedureCell referencing the invokable procedure in the passed plan */
  def planSelf()(implicit plan : PlanWriter) : ps.TempValue

  /** Optionally returns the native symbol for the procedure
    *
    * This is used for generating error messages. For some procedure values this will be unknown.
    */
  def nativeSymbolOpt(implicit plan : PlanWriter) : Option[String]
}
