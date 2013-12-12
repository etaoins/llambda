package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.planner.{step => ps}

trait InvokableProcedure {
  val signature : ProcedureSignature

  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue
  def planSelf()(implicit plan : PlanWriter) : ps.TempValue
}
