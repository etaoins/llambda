package llambda.planner

import llambda.ProcedureSignature
import llambda.planner.{step => ps}

trait InvokableProcedure {
  val signature : ProcedureSignature

  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue
  def planClosure()(implicit plan : PlanWriter) : ps.TempValue
}
