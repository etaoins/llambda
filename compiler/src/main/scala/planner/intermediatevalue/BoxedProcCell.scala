package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, ProcedureTypeToAdaptedSignature, BoxedValue, InvokableProc}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

/** Represents a boxed procedure cell
  *
  * These can be invoked by loading the procedure's entry point and passing it the procedure cell.
  */
class BoxedProcCell(
    procedureType: vt.ProcedureType,
    tempValue: ps.TempValue
) extends CellValue(procedureType, BoxedValue(ct.ProcedureCell, tempValue)) with ProcedureValue {
  val polyProcedureType = procedureType.toPolymorphic
  val hasSelfArg = true

  val signature = ProcedureTypeToAdaptedSignature(procedureType)

  def nativeSymbolOpt(implicit plan: PlanWriter) =
    None

  def withSelfTemp(selfTemp: ps.TempValue) =
    new BoxedProcCell(procedureType, selfTemp)

  override def toProcedureValue()(implicit plan: PlanWriter): ProcedureValue =
    this

  def planInvokableProc()(implicit plan: PlanWriter): InvokableProc = {
    val entryPointTemp = ps.TempValue()
    plan.steps += ps.LoadProcedureEntryPoint(entryPointTemp, tempValue, signature)

    InvokableProc(signature, entryPointTemp, Some(tempValue))
  }
}
