package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.{intermediatevalue => iv}

object DynamicProcPlanner extends ReportProcPlanner {
  override def planWithValue(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-parameter", List(initialValue)) =>
      val resultTemp = ps.Temp(vt.TopProcedureType)
      val initialValueTemp = initialValue._2.toTempValue(vt.AnySchemeType)

      plan.steps += ps.CreateParameterProc(resultTemp, initialValueTemp, None)

      Some(new iv.KnownParameterProc(resultTemp, hasConverter=false))
    
    case ("make-parameter", List(initialValue, converterProc)) =>
      val resultTemp = ps.Temp(vt.TopProcedureType)
      val initialValueTemp = initialValue._2.toTempValue(vt.AnySchemeType)

      val converterTemp = plan.withContextLocation(converterProc._1) {
        val converterProcType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        )

        converterProc._2.toTempValue(converterProcType)
      }

      plan.steps += ps.CreateParameterProc(resultTemp, initialValueTemp, Some(converterTemp))

      Some(new iv.KnownParameterProc(resultTemp, hasConverter=true))

    case _ =>
      None
  }
}
