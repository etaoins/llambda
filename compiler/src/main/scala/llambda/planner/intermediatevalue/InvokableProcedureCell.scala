package llambda.planner.intermediatevalue

import llambda.planner.{step => ps}
import llambda.{celltype => ct}
import llambda.planner.{PlanWriter, InvokableProcedure}
import llambda.codegen.AdaptedProcedureSignature

class InvokableProcedureCell(tempValue : ps.TempValue) extends InvokableProcedure {
  val signature = AdaptedProcedureSignature
  
  def planClosure()(implicit plan : PlanWriter) : ps.TempValue = {
    val closureTemp = new ps.TempValue
    plan.steps += ps.StoreProcedureClosure(closureTemp, tempValue)

    closureTemp
  }
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = new ps.TempValue
    plan.steps += ps.StoreProcedureEntryPoint(entryPointTemp, tempValue)

    entryPointTemp
  }
}

