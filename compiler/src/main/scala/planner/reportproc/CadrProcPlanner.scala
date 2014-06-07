package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("car" | "cdr", singleOperand :: Nil) =>
      val pairTemp = plan.withSourceLocation(singleOperand._1) {
        singleOperand._2.toTempValue(vt.IntrinsicCellType(ct.PairCell))
      }

      val resultTemp = ps.CellTemp(ct.DatumCell)

      if (reportName == "car") {
        plan.steps += ps.LoadPairCar(resultTemp, pairTemp)
      }
      else {
        plan.steps += ps.LoadPairCdr(resultTemp, pairTemp)
      }

      val possibleTypes = ct.DatumCell.concreteTypes
      val resultValue = new iv.IntrinsicCellValue(possibleTypes, ct.DatumCell, resultTemp)

      Some(PlanResult(
        state=initialState,
        value=resultValue
      ))

    case _ =>
      None
  }
}
