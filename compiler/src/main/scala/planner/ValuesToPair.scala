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
    */
  def apply(
      carValue : iv.IntermediateValue,
      cdrValue : iv.IntermediateValue,
      listLengthOpt : Option[Int]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    if (plan.config.schemeDialect.pairsAreImmutable) {
      (carValue, cdrValue) match {
        case (constantCar : iv.ConstantValue, constantCdr : iv.ConstantValue) =>
          // We can make this a constant pair
          return new iv.ConstantPairValue(constantCar, constantCdr)

        case _ =>
      }
    }

    val pairTemp = ps.CellTemp(ct.PairCell)

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

    plan.steps += ps.InitPair(pairTemp, safeListLengthOpt)
    plan.steps += ps.SetPairCar(pairTemp, carTemp)
    plan.steps += ps.SetPairCdr(pairTemp, cdrTemp)

    val resultValue = if (plan.config.schemeDialect.pairsAreImmutable) {
      // This pair is constant and we can optimise based on that
      new iv.KnownPairCellValue(carValue, cdrValue, pairTemp)
    }
    else {
      // This pair can be mutated at any time by (set-car!) or (set-cdr!) even if it's an immutable storage location
      TempValueToIntermediate(vt.AnyPairType, pairTemp)(plan.config)
    }

    resultValue
  }
}
