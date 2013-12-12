package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.codegen.AdaptedProcedureSignature

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

