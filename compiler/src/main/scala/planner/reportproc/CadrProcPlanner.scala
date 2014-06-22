package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("car" | "cdr", singleOperand :: Nil) =>
      val pairTemp = plan.withContextLocation(singleOperand._1) {
        singleOperand._2.toTempValue(vt.PairType)
      }

      val resultTemp = ps.CellTemp(ct.DatumCell)

      if (reportName == "car") {
        plan.steps += ps.LoadPairCar(resultTemp, pairTemp)
      }
      else {
        plan.steps += ps.LoadPairCdr(resultTemp, pairTemp)
      }

      val resultValue = new iv.CellValue(vt.AnySchemeType, ct.DatumCell, resultTemp)

      Some(PlanResult(
        state=initialState,
        value=resultValue
      ))

    case _ =>
      None
  }
}
