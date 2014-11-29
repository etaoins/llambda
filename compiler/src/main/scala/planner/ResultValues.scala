package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.{ImpossibleTypeConversionException, InternalCompilerErrorException}
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
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue

  /** Returns a multiple vlaue list containing the values in this result */
  def toMultipleValueList()(implicit plan : PlanWriter) : iv.IntermediateValue

  /** Returns the actual return type for this result value */
  def returnType : vt.ReturnType.ReturnType

  /** Returns the preferred return type for this result value */
  def preferredReturnType : vt.ReturnType.ReturnType

  /** Returns a TempValue representing the result in the appropriate representation for the ReturnType
    *
    * If void should be returned from the function then the result will not be defined
    */
  def toReturnTempValue(
      returnType : vt.ReturnType.ReturnType
  )(implicit plan : PlanWriter) : Option[ps.TempValue] = returnType match {
    case vt.ReturnType.SingleValue(vt.UnitType) =>
      None

    case vt.ReturnType.SingleValue(resultType) =>
      Some(toSingleValue().toTempValue(resultType))

    case vt.ReturnType.MultipleValues(valueListType) =>
      Some(toMultipleValueList().toTempValue(valueListType))
  }

  def withReturnType(newReturnType : vt.ReturnType.ReturnType) : ResultValues
}

case class SingleValue(value : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue =
    value

  def toMultipleValueList()(implicit plan : PlanWriter) =
    ValuesToList(List(value), capturable=false)
  
  def returnType = 
    vt.ReturnType.SingleValue(value.schemeType)

  def preferredReturnType = 
    vt.ReturnType.SingleValue(value.preferredRepresentation)
  
  def withReturnType(newReturnType : vt.ReturnType.ReturnType) : ResultValues = newReturnType.toValueListType match {
    case vt.SpecificPairType(
      vt.DirectSchemeTypeRef(newSingleValueType),
      vt.DirectSchemeTypeRef(vt.EmptyListType)
    ) =>
      SingleValue(value.withSchemeType(newSingleValueType))

    case _ =>
      throw new InternalCompilerErrorException("Attempted to retype result values with incompatible type")
  }
}

case class MultipleValues(multipleValueList : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue = {
    // This is tricky - we need to make sure that we have exactly one value
    // Make sure we're not empty
    val notEnoughValuesMessage = RuntimeErrorMessage(
      name="zeroValuesForSingle",
      text="Single value expected; 0 values provided"
    )

    val carValue = PlanCadr.loadCar(multipleValueList, Some(notEnoughValuesMessage)) 
    
    val extraValuesMessage = RuntimeErrorMessage(
      name="extraValuesForSingle",
      text="Single value expected; multiple values provided"
    )

    // We definitely have a pair now
    val newListSchemeType = multipleValueList.schemeType & vt.AnyPairType
    val retypedMultipleValueList = multipleValueList.withSchemeType(newListSchemeType)

    // Make sure there's no more values
    val cdrValue = PlanCadr.loadCdr(retypedMultipleValueList)
    cdrValue.castToSchemeType(vt.EmptyListType, Some(extraValuesMessage))

    carValue
  }
  
  def toMultipleValueList()(implicit plan : PlanWriter) =
    multipleValueList

  def returnType =
    vt.ReturnType.MultipleValues(multipleValueList.schemeType)

  def preferredReturnType =
    returnType
  
  def withReturnType(newReturnType : vt.ReturnType.ReturnType) : ResultValues = 
    MultipleValues(multipleValueList.withSchemeType(newReturnType.toValueListType))
}

object ResultValues {
  def apply(
      values : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) = values match {
    case List(singleValue) =>
      SingleValue(singleValue)

    case multipleValues =>
      val multipleValueList = ValuesToList(multipleValues, capturable=false)
      MultipleValues(multipleValueList)
  }
}
