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

  def returnType : vt.ReturnType.ReturnType[vt.ValueType] =
    vt.ReturnType.SingleValue(value.schemeType)

  def preferredReturnType =
    vt.ReturnType.SingleValue(value.preferredRepresentation)

  def withReturnType(newReturnType : vt.ReturnType.ReturnType[vt.SchemeType]) : ResultValues =
    SingleValue(value.withSchemeType(newReturnType.schemeType))

  def castToReturnType(
      targetType : vt.ReturnType.ReturnType[vt.SchemeType]
  )(implicit plan : PlanWriter) : ResultValues = {
    SingleValue(value.castToSchemeType(targetType.schemeType))
  }
}

/** Return value from a procedure that cannot return
  *
  * This can safely be treated as a unit value but we can special case this for optimisation purposes
  */
object UnreachableValue extends ResultValues {
  def toSingleValue()(implicit plan : PlanWriter) : iv.IntermediateValue =
    iv.UnitValue

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
      // VALUESTODO: This shouldn't be possible - change this signature
      throw new InternalCompilerErrorException("Multiple values unsupported")
  }
}
