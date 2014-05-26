package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import llambda.compiler.codegen.AdaptedProcedureSignature

object ApplyProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("apply", List((procSourceLoc, procValue), (_, argListValue))) if argListValue.isDefiniteProperList =>
      // Convert to a procedure cell so we can use its trampoline
      val procTemp = LocateExceptionsWith(procSourceLoc) {
        procValue.toTempValue(vt.IntrinsicCellType(ct.ProcedureCell))
      }

      // Load the entry point
      val entryPointTemp = ps.EntryPointTemp()
      plan.steps += ps.StoreProcedureEntryPoint(entryPointTemp, procTemp)

      // Prepare the arguments
      val restArgTemp = argListValue.toTempValue(vt.IntrinsicCellType(ct.ListElementCell))
      val allArgs = List(worldPtr, procTemp, restArgTemp).map(ps.InvokeArgument(_))

      // Prepare the result
      val resultTemp = ps.CellTemp(ct.DatumCell)

      plan.steps += ps.Invoke(Some(resultTemp), AdaptedProcedureSignature, entryPointTemp, allArgs)

      Some(PlanResult(
        state=state,
        value=TempValueToIntermediate(AdaptedProcedureSignature.returnType.get, resultTemp)
      ))

    case _ =>
      None
  }
}
