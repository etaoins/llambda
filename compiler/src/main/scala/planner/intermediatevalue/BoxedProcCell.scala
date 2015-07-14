package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, ApplicableTypeToAdaptedSignature, BoxedValue}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

/** Represents a boxed procedure cell
  *
  * These can be invoked by loading the procedure's entry point and passing it the procedure cell.
  */
class BoxedProcCell(
    applicableType : vt.ApplicableType,
    tempValue : ps.TempValue
) extends CellValue(applicableType, BoxedValue(ct.ProcedureCell, tempValue)) with InvokableProc {
  val signature = ApplicableTypeToAdaptedSignature(applicableType)
  val polySignature = signature.toPolymorphic

  def planSelf()(implicit plan : PlanWriter) : ps.TempValue =
    tempValue

  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.LoadProcedureEntryPoint(entryPointTemp, tempValue, signature)

    entryPointTemp
  }

  def nativeSymbolOpt(implicit plan : PlanWriter) =
    None

  def withSelfTemp(selfTemp : ps.TempValue) =
    new BoxedProcCell(applicableType, selfTemp)

  override def toInvokableProc()(implicit plan : PlanWriter) : InvokableProc =
    this
}

