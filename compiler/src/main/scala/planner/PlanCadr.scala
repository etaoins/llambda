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
      pairValue : iv.IntermediateValue,
      errorMessageOpt : Option[RuntimeErrorMessage],
      knownPairLoader : KnownPairLoader,
      typeRefForPairType : TypeRefForPairType,
      planLoader : PlanLoader
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = pairValue match {
    case knownPair : iv.KnownPair =>
      // We can resolve this at compile time
      knownPairLoader(knownPair)

    case _ =>
      val pairTemp = pairValue.toTempValue(vt.AnyPairType, errorMessageOpt)

      // Does this pair have a specific pair type?
      val resultType = (pairValue.schemeType & vt.AnyPairType)  match {
        case pairType : vt.PairType =>
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

  def loadCar(
      pairValue : iv.IntermediateValue,
      errorMessageOpt : Option[RuntimeErrorMessage] = None
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    loadCadr(pairValue, errorMessageOpt, _.car, _.carTypeRef, ps.LoadPairCar)
  }
  
  def loadCdr(
      pairValue : iv.IntermediateValue,
      errorMessageOpt : Option[RuntimeErrorMessage] = None
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    loadCadr(pairValue, errorMessageOpt, _.cdr, _.cdrTypeRef, ps.LoadPairCdr)
  }
}
