package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure, ProcedureTypeToAdaptedSignature}
import llambda.compiler.{valuetype => vt}

class InvokableProcedureCell(tempValue : ps.TempValue) extends InvokableProcedure {
  val signature = ProcedureTypeToAdaptedSignature(vt.TopProcedureType)
  
  def planSelf()(implicit plan : PlanWriter) : ps.TempValue = 
    tempValue
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.LoadProcedureEntryPoint(entryPointTemp, tempValue, signature)

    entryPointTemp
  }

  def nativeSymbolOpt(implicit plan : PlanWriter) = 
    None
}

