package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import llambda.compiler.codegen.AdaptedProcedureSignature

object ApplyProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("apply", List((procContextLoc, procValue), (_, argListValue))) if argListValue.isDefiniteProperList =>
      // Convert to a procedure cell so we can use its trampoline
      val procTemp = plan.withContextLocation(procContextLoc) {
        procValue.toTempValue(vt.ProcedureType)
      }

      // Load the entry point
      val entryPointTemp = ps.EntryPointTemp()
      plan.steps += ps.LoadProcedureEntryPoint(entryPointTemp, procTemp)

      // Prepare the arguments
      val restArgTemp = argListValue.toTempValue(vt.ListElementType)
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
