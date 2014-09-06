package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CadrProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(
      reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("car" | "cdr", List((_, knownPair : iv.KnownPair))) =>
      // We can resolve this at compile time
      val resultValue = if (reportName == "car") {
        knownPair.car
      }
      else {
        knownPair.cdr
      }

      Some(SingleValue(resultValue))

    case ("car" | "cdr", List(singleOperand)) =>
      val pairValue = singleOperand._2

      val pairTemp = plan.withContextLocation(singleOperand._1) {
        pairValue.toTempValue(vt.AnyPairType)
      }

      // Does this pair have a specific pair type?
      val resultType = pairValue.schemeType match {
        case pairType : vt.PairType if reportName == "car" =>
          pairType.unrollChildTypeRef(pairType.carTypeRef)

        case pairType : vt.PairType if reportName == "cdr" =>
          pairType.unrollChildTypeRef(pairType.cdrTypeRef)

        case _ =>
          vt.AnySchemeType
      }

      val resultTemp = ps.CellTemp(ct.AnyCell)

      if (reportName == "car") {
        plan.steps += ps.LoadPairCar(resultTemp, pairTemp)
      }
      else {
        plan.steps += ps.LoadPairCdr(resultTemp, pairTemp)
      }

      val resultValue = new iv.CellValue(resultType, BoxedValue(ct.AnyCell, resultTemp))

      Some(SingleValue(resultValue))

    case _ =>
      None
  }
}
