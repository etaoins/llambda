package llambda.planner.intermediatevalue

import llambda.planner.{step => ps}
import llambda.{boxedtype => bt}
import llambda.planner.{PlanWriter, InvokableProcedure}
import llambda.codegen.BoxedProcedureSignature

class InvokableBoxedProcedure(tempValue : ps.TempValue) extends InvokableProcedure {
  val signature = BoxedProcedureSignature
  
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

