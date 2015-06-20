package io.llambda.compiler.valuetype
import io.llambda

import Implicits._

object ReturnType {
  /** Represents the return type of a procedure */
  sealed abstract trait ReturnType[+T >: SchemeType <: ValueType] {
    /** Returns the representation used internally to contain this return type
      *
      * If no representation is required then the result is not defined
      */
    def representationTypeOpt : Option[T]

    /** Returns proper list type contain the Scheme types of the return values
      *
      * This is useful for type checking or type arithmetic on return types
      */
    def toValueListType : SchemeType

    override def toString =
      NameForReturnType(this)

    /** Returns the Scheme return type for this return type
      *
      * This is only useful if T is set to ValueType
      */
    def schemeReturnType : ReturnType[SchemeType]
  }

  /** Represents a single value returned from a procedure
    *
    * This is the normal case. Single values are represented by the their native type without any wrapping
    */
   case class SingleValue[+T >: SchemeType <: ValueType](valueType : T) extends ReturnType[T] {
    def representationTypeOpt = valueType match {
      case UnitType =>
        None

      case otherType =>
        Some(otherType)
    }

    def toValueListType : SchemeType =
      SpecificPairType(
        carTypeRef=DirectSchemeTypeRef(valueType.schemeType),
        cdrTypeRef=DirectSchemeTypeRef(EmptyListType)
      )

    def schemeReturnType : ReturnType[SchemeType] =
      SingleValue(valueType.schemeType)
  }

  /** Represents multiple values returned from a procedure
    *
    * This is internally represented as a proper list of values. This makes the implementation of (values) and
    * (call-by-values) simpler
    */
  case class MultipleValues(valueListType : SchemeType) extends ReturnType[SchemeType] {
    def representationType : SchemeType = valueListType

    def representationTypeOpt : Option[SchemeType] =
      Some(representationType)

    def toValueListType : SchemeType =
      valueListType

    def schemeReturnType : ReturnType[SchemeType] =
      this
  }

  /** Represents a fixed number of return values of specific types */
  object SpecificValues {
    def apply(valueTypes : List[SchemeType]) = valueTypes match {
      case List(singleValue) =>
        SingleValue(singleValue)

      case multipleValues =>
        MultipleValues(
          SpecificProperListType(multipleValues)
        )
    }
  }

  /** Represents an arbitrary number of return values of any type */
  object ArbitraryValues extends MultipleValues(UniformProperListType(AnySchemeType))

  /** Represents the return type of a procedure that cannot return
    *
    * This is distinct from the NoReturn ProcedureSignature attribute as its possible for native functions to have
    * non-void return types and also not return.
    */
  object UnreachableValue extends ReturnType[SchemeType] {
    def representationTypeOpt = None

    def toValueListType : SchemeType =
      SpecificPairType(
        carTypeRef=DirectSchemeTypeRef(UnitType),
        cdrTypeRef=DirectSchemeTypeRef(EmptyListType)
      )

    def schemeReturnType : ReturnType[SchemeType] =
      this
  }
}
