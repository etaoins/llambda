package llambda.planner.intermediatevalue

import llambda.planner.{step => ps}
import llambda.{celltype => ct}
import llambda.planner.{PlanWriter, InvokableProcedure}
import llambda.codegen.AdaptedProcedureSignature

class InvokableProcedureCell(tempValue : ps.TempValue) extends InvokableProcedure {
  val signature = AdaptedProcedureSignature
  
  def planSelf()(implicit plan : PlanWriter) : ps.TempValue = 
    tempValue
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = new ps.TempValue
    plan.steps += ps.StoreProcedureEntryPoint(entryPointTemp, tempValue)

    entryPointTemp
  }
}

