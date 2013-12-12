package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operandValues : List[iv.IntermediateValue])(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, operandValues) match {
    case ("car" | "cdr", singleOperand :: Nil) =>
      val pairTemp = singleOperand.toRequiredTempValue(vt.IntrinsicCellType(ct.PairCell))
      val resultTemp = new ps.TempValue

      if (reportName == "car") {
        plan.steps += ps.StorePairCar(resultTemp, pairTemp)
      }
      else {
        plan.steps += ps.StorePairCdr(resultTemp, pairTemp)
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
