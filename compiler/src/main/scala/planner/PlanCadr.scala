package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ContextLocated, RuntimeErrorMessage}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanCadr {
  private type KnownPairLoader = (iv.KnownPair) => iv.IntermediateValue
  private type TypeRefForPairType = (vt.PairType) => vt.SchemeTypeRef
  private type PlanLoader = (ps.TempValue, ps.TempValue) => ps.Step

  private def loadCadr(
      pairValue: iv.IntermediateValue,
      emptyListMessageOpt: Option[RuntimeErrorMessage] = None,
      improperListMessageOpt: Option[RuntimeErrorMessage] = None,
      knownPairLoader: KnownPairLoader,
      typeRefForPairType: TypeRefForPairType,
      planLoader: PlanLoader
  )(implicit plan: PlanWriter): iv.IntermediateValue = pairValue match {
    case knownPair: iv.KnownPair =>
      // We can resolve this at compile time
      knownPairLoader(knownPair)

    case _ =>
      val listValue = if (improperListMessageOpt.isDefined) {
        // XXX: Once #50 is fixed castToSchemeType can intersect the value's type internally and we can skip the manual
        // intersection below
        pairValue.castToSchemeType(vt.ListElementType, improperListMessageOpt)
        pairValue.withSchemeType(pairValue.schemeType & vt.ListElementType)
      }
      else {
        pairValue
      }

      val pairTemp = listValue.toTempValue(vt.AnyPairType, emptyListMessageOpt)

      // Does this pair have a specific pair type?
      val resultType = (listValue.schemeType & vt.AnyPairType) match {
        case pairType: vt.PairType =>
          val rawResultType = pairType.unrollChildTypeRef(typeRefForPairType(pairType))

          // ValuesToPair will always convert to TopProcedureType
          rawResultType.replaceApplicableType(vt.TopProcedureType)

        case _ =>
          vt.AnySchemeType
      }

      val resultTemp = ps.CellTemp(ct.AnyCell)
      plan.steps += planLoader(resultTemp, pairTemp)

      val resultValue = new iv.CellValue(resultType, BoxedValue(ct.AnyCell, resultTemp))
      resultValue
  }

  /** Loads the car from the passed pair value
    *
    * @param  pairValue               Pair value to load the car of
    * @param  emptyListMessageOpt     Error to signal if the passed pair is the empty list. This defaults to a generic
    *                                 type conversion error
    * @param  improperListMessageOpt  Error to signal if the passed pair is neither a pair or empty list. This defaults
    *                                 to emptyListMessageOpt
    */
  def loadCar(
      pairValue: iv.IntermediateValue,
      emptyListMessageOpt: Option[RuntimeErrorMessage] = None,
      improperListMessageOpt: Option[RuntimeErrorMessage] = None
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    loadCadr(pairValue, emptyListMessageOpt, improperListMessageOpt, _.car, _.carTypeRef, ps.LoadPairCar)
  }

  /** Loads the cdr from the passed pair value
    *
    * @param  pairValue               Pair value to load the cdr of
    * @param  emptyListMessageOpt     Error to signal if the passed pair is the empty list. This defaults to a generic
    *                                 type conversion error
    * @param  improperListMessageOpt  Error to signal if the passed pair is neither a pair or empty list. This defaults
    *                                 to emptyListMessageOpt
    */
  def loadCdr(
      pairValue: iv.IntermediateValue,
      emptyListMessageOpt: Option[RuntimeErrorMessage] = None,
      improperListMessageOpt: Option[RuntimeErrorMessage] = None
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    loadCadr(pairValue, emptyListMessageOpt, improperListMessageOpt, _.cdr, _.cdrTypeRef, ps.LoadPairCdr)
  }
}
