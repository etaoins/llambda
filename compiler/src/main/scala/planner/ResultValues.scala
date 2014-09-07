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
  def toSingleValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue

  /** Returns a multiple vlaue list containing the values in this result */
  def toMultipleValueList()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue

  /** Returns the actual return type for this result value */
  def returnType : ReturnType.ReturnType

  /** Returns the preferred return type for this result value */
  def preferredReturnType : ReturnType.ReturnType

  /** Returns a TempValue representing the result in the appropriate representation for the ReturnType
    *
    * If void should be returned from the function then the result will not be defined
    */
  def toReturnTempValue(
      returnType : ReturnType.ReturnType
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ps.TempValue] = returnType match {
    case ReturnType.SingleValue(vt.UnitType) =>
      None

    case ReturnType.SingleValue(resultType) =>
      Some(toSingleValue().toTempValue(resultType))

    case ReturnType.MultipleValues(valueListType) =>
      Some(toMultipleValueList().toTempValue(valueListType))
  }
}

case class SingleValue(value : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue =
    value

  def toMultipleValueList()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) =
    ValuesToProperList(List(value))
  
  def returnType = 
    ReturnType.SingleValue(value.schemeType)

  def preferredReturnType = 
    ReturnType.SingleValue(value.preferredRepresentation)
}

case class MultipleValues(multipleValueList : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    // This is tricky - we need to make sure that we have exactly one value
    // Make sure we're not empty
    val notEnoughValuesMessage = RuntimeErrorMessage(
      name="zeroValuesForSingle",
      text="Single value expected; 0 values provided"
    )

    val carValue = PlanCadr.loadCar(plan.activeContextLocated, multipleValueList, Some(notEnoughValuesMessage)) 
    
    val extraValuesMessage = RuntimeErrorMessage(
      name="extraValuesForSingle",
      text="Single value expected; multiple values provided"
    )

    // We definitely have a pair now
    val newListSchemeType = multipleValueList.schemeType & vt.AnyPairType
    val retypedMultipleValueList = multipleValueList.withSchemeType(newListSchemeType)

    // Make sure there's no more values
    val cdrValue = PlanCadr.loadCdr(plan.activeContextLocated, retypedMultipleValueList)
    cdrValue.castToSchemeType(vt.EmptyListType)

    carValue
  }
  
  def toMultipleValueList()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) =
    multipleValueList

  def returnType =
    ReturnType.MultipleValues(multipleValueList.schemeType)

  def preferredReturnType =
    returnType
}

object ResultValues {
  def apply(
      values : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) = values match {
    case List(singleValue) =>
      SingleValue(singleValue)

    case multipleValues =>
      val multipleValueList = ValuesToProperList(multipleValues)
      MultipleValues(multipleValueList)
  }
}
