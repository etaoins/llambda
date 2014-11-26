package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

private[planner] object ValuesToPair {
  /** Converts a car and cdr value to a pair
    *
    * This automatically takes advantage of immutable pair support to build KnownPairCellValue instances when the Scheme
    * dialect allows it
    *
    * @param  carValue       car value for the new pair
    * @param  cdrValue       cdr value for the new pair
    * @param  listLengthOpt  Optional length of the proper list this pair is the head of. This is ignored if the Scheme
    *                        dialect uses mutable pairs
    * @param  capturable     Indicates if this pair can be captured to a storage location during the lifetime of the
    *                        returned IntermediateValue
    */
  def apply(
      carValue : iv.IntermediateValue,
      cdrValue : iv.IntermediateValue,
      listLengthOpt : Option[Int],
      capturable : Boolean = true
  )(implicit plan : PlanWriter) : iv.IntermediateValue = {
    if (plan.config.schemeDialect.pairsAreImmutable) {
      (carValue, cdrValue) match {
        case (constantCar : iv.ConstantValue, constantCdr : iv.ConstantValue) =>
          // We can make this a constant pair
          return new iv.ConstantPairValue(constantCar, constantCdr)

        case _ =>
      }
    }

    val preserveTypes = plan.config.schemeDialect.pairsAreImmutable || !capturable

    val carTemp = carValue.toTempValue(vt.AnySchemeType)
    val cdrTemp = cdrValue.toTempValue(vt.AnySchemeType)

    // We can only encode the list length if we're using immutable pairs
    // Otherwise any modification to the list's pair could invalidate the information
    val safeListLengthOpt = if (plan.config.schemeDialect.pairsAreImmutable) {
      listLengthOpt
    }
    else {
      None
    }

    val pairTemp = ps.CellTemp(ct.PairCell)
    plan.steps += ps.InitPair(pairTemp, safeListLengthOpt)
    plan.steps += ps.SetPairCar(pairTemp, carTemp)
    plan.steps += ps.SetPairCdr(pairTemp, cdrTemp)

    val resultValue = if (preserveTypes) {
      // Note that when we converted to an vt.AnySchemeType TempValue we converted any procedure type we had to the
      // TopProceduretype
      // This is actually what we want - otherwise it would be very complicated to convert procedure typed lists
      // between each other
      var storedType = vt.PairType(
        carTypeRef=vt.DirectSchemeTypeRef(carValue.schemeType.replaceApplicableType(vt.TopProcedureType)),
        cdrTypeRef=vt.DirectSchemeTypeRef(cdrValue.schemeType.replaceApplicableType(vt.TopProcedureType))
      )

      // This pair is immutable and we can optimise based on that
      new iv.KnownPairCellValue(carValue, cdrValue, storedType, pairTemp)
    }
    else {
      // This pair can be mutated at any time by (set-car!) or (set-cdr!) even if it's an immutable storage location
      TempValueToIntermediate(vt.AnyPairType, pairTemp)(plan.config)
    }

    resultValue
  }
}
