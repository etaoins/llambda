package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ListProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("list", operands) =>
      // Build a proper list directly
      val resultTemp = ps.CellTemp(ct.ListElementCell)
      val operandTemps = operands.map(_._2).map(_.toTempValue(vt.IntrinsicCellType(ct.DatumCell)))

      plan.steps += ps.BuildProperList(resultTemp, operandTemps) 
      
      val listValue = new iv.IntrinsicCellValue(
        possibleTypes=ct.ListElementCell.concreteTypes,
        cellType=ct.ListElementCell,
        tempValue=resultTemp,
        properListCell=true 
      )

      Some(PlanResult(
        state=initialState,
        value=listValue
      ))

    case _ =>
      None
  }
}
