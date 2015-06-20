package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.InternalCompilerErrorException
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
  def returnType : vt.ReturnType.ReturnType[vt.ValueType]

  /** Returns the preferred return type for this result value */
  def preferredReturnType : vt.ReturnType.ReturnType[vt.ValueType]

  /** Returns a TempValue representing the result in the appropriate representation for the ReturnType
    *
    * If void should be returned from the function then the result will not be defined
    */
  def toReturnTempValue(
      returnType : vt.ReturnType.ReturnType[vt.ValueType]
  )(implicit plan : PlanWriter) : Option[ps.TempValue] = returnType match {
    case vt.ReturnType.SingleValue(vt.UnitType) | vt.ReturnType.UnreachableValue =>
      None

    case vt.ReturnType.SingleValue(resultType) =>
      Some(toSingleValue().toTempValue(resultType))

    case vt.ReturnType.MultipleValues(valueListType) =>
      Some(toMultipleValueList().toTempValue(valueListType))
  }

  def withReturnType(newReturnType : vt.ReturnType.ReturnType[vt.SchemeType]) : ResultValues

  /** Casts this value to to another ReturnType
    *
    * This works analogously to IntermediateValue.toSchemeType(). If the value can't satisfy the target type then a
    * TypeException will be thrown. If the value may satisfy the target type then a runtime type check will be planned.
    */
  def castToReturnType(targetType : vt.ReturnType.ReturnType[vt.SchemeType])(implicit plan : PlanWriter) : ResultValues
}

case class SingleValue(value : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue =
    value

  def toMultipleValueList()(implicit plan : PlanWriter) =
    ValuesToList(List(value), capturable=false)

  def returnType : vt.ReturnType.ReturnType[vt.ValueType] =
    vt.ReturnType.SingleValue(value.schemeType)

  def preferredReturnType =
    vt.ReturnType.SingleValue(value.preferredRepresentation)

  def withReturnType(newReturnType : vt.ReturnType.ReturnType[vt.SchemeType]) : ResultValues =
    newReturnType.toValueListType match {
      case vt.SpecificPairType(
        vt.DirectSchemeTypeRef(newSingleValueType),
        vt.DirectSchemeTypeRef(vt.EmptyListType)
      ) =>
        SingleValue(value.withSchemeType(newSingleValueType))

      case _ =>
        throw new InternalCompilerErrorException("Attempted to retype result values with incompatible type")
    }

  def castToReturnType(
      targetType : vt.ReturnType.ReturnType[vt.SchemeType]
  )(implicit plan : PlanWriter) : ResultValues = {
    targetType.toValueListType  match {
      case vt.SpecificPairType(
        vt.DirectSchemeTypeRef(newSingleValueType),
        vt.DirectSchemeTypeRef(vt.EmptyListType)
      ) =>
        SingleValue(value.castToSchemeType(newSingleValueType))

      case otherListType =>
        if (vt.SatisfiesType(otherListType, vt.SpecificProperListType(List(value.schemeType))) == Some(true)) {
          // We already satisfy the type; don't convert ourselves to MultipleValues which doesn't optimise as well
          this
        }
        else {
          MultipleValues(ValuesToList(List(value), capturable=false).castToSchemeType(otherListType))
        }
    }
  }
}

case class MultipleValues(multipleValueList : iv.IntermediateValue) extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue = {
    // This is tricky - we need to make sure that we have exactly one value
    // Make sure we're not empty
    val notEnoughValuesMessage = RuntimeErrorMessage(
      category=ErrorCategory.Arity,
      name="zeroValuesForSingle",
      text="Single value expected; 0 values provided"
    )

    val carValue = PlanCadr.loadCar(multipleValueList, Some(notEnoughValuesMessage))

    val extraValuesMessage = RuntimeErrorMessage(
      category=ErrorCategory.Arity,
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

  def withReturnType(newReturnType : vt.ReturnType.ReturnType[vt.SchemeType]) : ResultValues =
    MultipleValues(multipleValueList.withSchemeType(newReturnType.toValueListType))

  def castToReturnType(
      targetType : vt.ReturnType.ReturnType[vt.SchemeType]
  )(implicit plan : PlanWriter) : ResultValues =
    MultipleValues(multipleValueList.castToSchemeType(targetType.toValueListType))
}

/** Return value from a procedure that cannot return
  *
  * This can safely be treated as a unit value but we can special case this for optimisation purposes
  */
object UnreachableValue extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue =
    iv.UnitValue

  def toMultipleValueList()(implicit plan : PlanWriter) =
    ValuesToList(List(iv.UnitValue), capturable=false)

  def withReturnType(newReturnType : vt.ReturnType.ReturnType[vt.SchemeType]) : ResultValues =
    this

  def castToReturnType(
      targetType : vt.ReturnType.ReturnType[vt.SchemeType]
  )(implicit plan : PlanWriter) : ResultValues =
    this

  def preferredReturnType = vt.ReturnType.UnreachableValue

  def returnType = vt.ReturnType.UnreachableValue
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
