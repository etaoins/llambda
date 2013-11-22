package llambda.planner.reportproc

import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.nfi
import llambda.planner._

object CadrProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operandValues : List[iv.IntermediateValue])(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, operandValues) match {
    case ("car" | "cdr", singleOperand :: Nil) =>
      val pairTemp = singleOperand.toRequiredTempValue(nfi.BoxedValue(bt.BoxedPair))
      val resultTemp = new ps.TempValue

      if (reportName == "car") {
        plan.steps += ps.StorePairCar(resultTemp, pairTemp)
      }
      else {
        plan.steps += ps.StorePairCdr(resultTemp, pairTemp)
      }

      val possibleTypes = bt.BoxedDatum.concreteTypes
      val resultValue = new iv.DynamicBoxedValue(possibleTypes, bt.BoxedDatum, resultTemp)

      Some(PlanResult(
        state=initialState,
        value=resultValue
      ))

    case _ =>
      None
  }
}
