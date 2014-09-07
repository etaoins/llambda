package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanCadr {
  private type KnownPairLoader = (iv.KnownPair) => iv.IntermediateValue
  private type TypeRefForPairType = (vt.PairType) => vt.SchemeTypeRef
  private type PlanLoader = (ps.TempValue, ps.TempValue) => ps.Step
  
  private def loadCadr(
      located : ContextLocated,
      pairValue : iv.IntermediateValue,
      knownPairLoader : KnownPairLoader,
      typeRefForPairType : TypeRefForPairType,
      planLoader : PlanLoader
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = pairValue match {
    case knownPair : iv.KnownPair =>
      // We can resolve this at compile time
      knownPairLoader(knownPair)

    case _ =>
      val pairTemp = plan.withContextLocation(located) {
        pairValue.toTempValue(vt.AnyPairType)
      }

      // Does this pair have a specific pair type?
      val resultType = pairValue.schemeType match {
        case pairType : vt.PairType =>
          pairType.unrollChildTypeRef(typeRefForPairType(pairType))

        case _ =>
          vt.AnySchemeType
      }

      val resultTemp = ps.CellTemp(ct.AnyCell)
      plan.steps += planLoader(resultTemp, pairTemp)

      val resultValue = new iv.CellValue(resultType, BoxedValue(ct.AnyCell, resultTemp))
      resultValue
  }

  def loadCar(
      located : ContextLocated,
      pairValue : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    loadCadr(located, pairValue, _.car, _.carTypeRef, ps.LoadPairCar)
  }
  
  def loadCdr(
      located : ContextLocated,
      pairValue : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    loadCadr(located, pairValue, _.cdr, _.cdrTypeRef, ps.LoadPairCdr)
  }
}
