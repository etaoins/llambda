package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.{intermediatevalue => iv}

/*
import llambda.compiler.OutOfBoundsException
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._*/

object DynamicProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("make-parameter", List(initialValue)) =>
      val resultTemp = ps.Temp(vt.ProcedureType)
      val initialValueTemp = initialValue._2.toTempValue(vt.AnySchemeType)

      plan.steps += ps.CreateParameterProc(worldPtr, resultTemp, initialValueTemp, None)

      Some(SingleValue(
        new iv.KnownParameterProc(resultTemp, hasConverter=false)
      ))
    
    case ("make-parameter", List(initialValue, converterProc)) =>
      val resultTemp = ps.Temp(vt.ProcedureType)
      val initialValueTemp = initialValue._2.toTempValue(vt.AnySchemeType)

      val converterTemp = plan.withContextLocation(converterProc._1) {
        converterProc._2.toTempValue(vt.ProcedureType)
      }

      plan.steps += ps.CreateParameterProc(worldPtr, resultTemp, initialValueTemp, Some(converterTemp))

      Some(SingleValue(
        new iv.KnownParameterProc(resultTemp, hasConverter=true)
      ))

    case _ =>
      None
  }
}
