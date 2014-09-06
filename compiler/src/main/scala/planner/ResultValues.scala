package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ReturnType, RuntimeErrorMessage}
import llambda.compiler.ImpossibleTypeConversionException
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

import llambda.compiler.valuetype.Implicits._

sealed abstract class ResultValues {
  /** Returns an intermediate value containing the single value in this result
    *
    * If the result contains multiple values then ImpossibleTypeConversionException will be thrown
    */
  def toIntermediateValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue

  /** Returns the actual return type for this result value */
  def returnType : ReturnType.ReturnType

  /** Returns the preferred return type for this result value */
  def preferredReturnType : ReturnType.ReturnType

  /** Returns a TempValue representing this result of the appropriate ReturnType
    *
    * If void should be returned from the function then the result will not be defined
    */
  def toReturnTempValue(
      returnType : ReturnType.ReturnType
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ps.TempValue]
}

case class SingleValue(value : iv.IntermediateValue) extends ResultValues {
  def toIntermediateValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue =
    value
  
  def returnType = 
    ReturnType.SingleValue(value.schemeType)

  def preferredReturnType = 
    ReturnType.SingleValue(value.preferredRepresentation)
  
  def toReturnTempValue(
      returnType : ReturnType.ReturnType
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ps.TempValue] = returnType match {
    case ReturnType.SingleValue(vt.UnitType) =>
      None

    case ReturnType.SingleValue(resultType) =>
      Some(value.toTempValue(resultType))

    case specificValues @ ReturnType.SpecificValues(valueTypes) =>
      if (valueTypes.length != 1) {
        throw new ImpossibleTypeConversionException(plan.activeContextLocated, s"${valueTypes.length} values expected; single value provided")
      }
      
      // Make sure the multiple value list is of the correct type
      val requiredType = specificValues.representationType
      Some(ValuesToProperList(List(value)).toTempValue(requiredType))

    case ReturnType.ArbitraryValues =>
      val requiredType = ReturnType.ArbitraryValues.representationType
      Some(ValuesToProperList(List(value)).toTempValue(requiredType))
  }
}

sealed abstract class MultipleValues extends ResultValues {
  /** TempValue containing the head of our multiple value list */
  val multipleValueList : BoxedValue

  def returnType : ReturnType.MultipleValues
  
  def toReturnTempValue(
      targetReturnType : ReturnType.ReturnType
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ps.TempValue] = targetReturnType match {
    case ReturnType.SingleValue(vt.UnitType) =>
      None

    case ReturnType.SingleValue(resultType) =>
      val singleValue = toIntermediateValue()
      val resultTemp = singleValue.toTempValue(resultType)

      Some(resultTemp)

    case specificValues @ ReturnType.SpecificValues(targetValueTypes) =>
      val requiredListType = specificValues.representationType
      
      // Ensure our list is of the correct type
      val multipleValueListType = returnType.representationType
      val multipleValueListValue = new iv.CellValue(multipleValueListType, multipleValueList)
      multipleValueListValue.castToSchemeType(requiredListType)

      Some(multipleValueList.castToCellTempValue(requiredListType.cellType))

    case ReturnType.ArbitraryValues =>
      val requiredListType = ReturnType.ArbitraryValues.representationType
      Some(multipleValueList.castToCellTempValue(requiredListType.cellType))
  }
}

case class SpecificValues(multipleValueList : BoxedValue, valueTypes : List[vt.SchemeType]) extends MultipleValues {
  def toIntermediateValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    if (valueTypes.length != 1) {
      throw new ImpossibleTypeConversionException(plan.activeContextLocated, s"Single value expected; ${valueTypes.length} values provided")
    }

    // We definitely have a pair
    val pairTemp = multipleValueList.castToCellTempValue(ct.PairCell)

    // Pull the value off the pair
    val valueType = valueTypes.head
    val valueTemp = ps.Temp(vt.AnySchemeType) 
    plan.steps += ps.LoadPairCar(valueTemp, pairTemp)

    val boxedValue = BoxedValue(ct.AnyCell, valueTemp)
    new iv.CellValue(valueType, boxedValue)
  }

  def returnType =
    ReturnType.SpecificValues(valueTypes)
  
  def preferredReturnType = 
    if (valueTypes.length == 1) {
      ReturnType.SingleValue(valueTypes.head)
    }
    else {
      ReturnType.SpecificValues(valueTypes)
    }
}


case class ArbitraryValues(multipleValueList : BoxedValue) extends MultipleValues {
  def toIntermediateValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    // This is tricky - we need to make sure that we have exactly one value
    
    val isPairTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.TestCellType(isPairTemp, multipleValueList.tempValue, ct.PairCell, Set(ct.PairCell, ct.EmptyListCell))

    // Make sure we're not empty
    val notEnoughValuesMessage = RuntimeErrorMessage(
      name="zeroValuesForSingle",
      text="Single value expected; 0 values provided"
    )
    plan.steps += ps.AssertPredicate(worldPtr, isPairTemp, notEnoughValuesMessage, Some(multipleValueList.tempValue))

    val pairTemp = multipleValueList.castToCellTempValue(ct.PairCell)

    // Make sure we don't have extra value
    val cdrValue = ps.Temp(vt.AnySchemeType)
    plan.steps += ps.LoadPairCdr(cdrValue, pairTemp)

    val isEmptyListTemp = ps.Temp(vt.Predicate) 
    plan.steps += ps.TestCellType(isEmptyListTemp, cdrValue, ct.EmptyListCell, Set(ct.PairCell, ct.EmptyListCell))
    
    val extraValuesMessage = RuntimeErrorMessage(
      name="extraValuesForSingle",
      text="Single value expected; multiple values provided"
    )
    plan.steps += ps.AssertPredicate(worldPtr, isEmptyListTemp, extraValuesMessage, Some(multipleValueList.tempValue))

    // Finally we can load the car
    val valueTemp = ps.Temp(vt.AnySchemeType)
    plan.steps += ps.LoadPairCar(valueTemp, pairTemp)

    val boxedValue = BoxedValue(ct.AnyCell, valueTemp)
    new iv.CellValue(vt.AnySchemeType, boxedValue) 
  }
  
  def returnType = 
    ReturnType.ArbitraryValues
  
  def preferredReturnType =
    ReturnType.ArbitraryValues
  
  def multipleValueListType =
    vt.UniformProperListType(vt.AnySchemeType)
}
