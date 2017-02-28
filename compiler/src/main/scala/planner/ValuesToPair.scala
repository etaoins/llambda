package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

private[planner] object ValuesToPair {
  /** Converts a car and cdr value to a pair
    *
    * This automatically takes advantage of immutable pair support to build KnownPairCellValue instances
    *
    * @param  carValue   car value for the new pair
    * @param  cdrValue   cdr value for the new pair
    */
  def apply(
      carValue: iv.IntermediateValue,
      cdrValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = (carValue, cdrValue) match {
    case (constantCar: iv.ConstantValue, constantCdr: iv.ConstantValue) =>
      // We can make this a constant pair
      iv.ConstantPairValue(constantCar, constantCdr)

    case _ =>
      val listLengthOpt = cdrValue match {
        case knownListElement: iv.KnownListElement =>
          knownListElement.listLengthOpt.map(_ + 1)

        case _ =>
          None
      }

      val carTemp = carValue.toTempValue(vt.AnySchemeType)
      val cdrTemp = cdrValue.toTempValue(vt.AnySchemeType)

      val pairTemp = ps.TempValue()
      plan.steps += ps.InitPair(pairTemp, carTemp, cdrTemp, listLengthOpt)

      // Note that when we converted to an vt.AnySchemeType TempValue we converted any procedure type we had to the
      // TopProceduretype
      // This is actually what we want - otherwise it would be very complicated to convert procedure typed lists
      // between each other
      var storedType = vt.PairType(
        carTypeRef=vt.DirectSchemeTypeRef(carValue.schemeType.replaceApplicableType(vt.TopProcedureType)),
        cdrTypeRef=vt.DirectSchemeTypeRef(cdrValue.schemeType.replaceApplicableType(vt.TopProcedureType))
      )

      new iv.KnownPairCellValue(carValue, cdrValue, storedType, pairTemp)
  }
}
