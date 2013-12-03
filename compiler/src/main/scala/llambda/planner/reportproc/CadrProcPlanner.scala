package llambda.planner.reportproc

import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.planner._

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
