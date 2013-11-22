package llambda.planner

import llambda.nfi
import llambda.planner.{step => ps}

trait InvokableProcedure {
  val signature : nfi.NativeSignature

  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue
}
