package io.llambda.compiler.valuetype
import io.llambda

import Implicits._

object ReturnType {
  /** Represents the return type of a procedure */
  sealed abstract trait ReturnType {
    /** Returns the representation used internally to contain this return type
      *
      * If no representation is required then the result is not defined
      */
    def representationTypeOpt : Option[ValueType]
  
    /** Returns proper list type contain the Scheme types of the return values
      *
      * This is useful for type checking or type arithmetic on return types
      */
    def toValueListType : SchemeType

    override def toString =
      NameForReturnType(this)
  }

  /** Represents a single value returned from a procedure 
    *
    * This is the normal case. Single values are represented by the their native type without any wrapping
    */
  case class SingleValue(valueType : ValueType) extends ReturnType {
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
  }

  /** Represents multiple values returned from a procedure
    *
    * This is internally represented as a proper list of values. This makes the implementation of (values) and 
    * (call-by-values) simpler
    */
  case class MultipleValues(valueListType : SchemeType) extends ReturnType {
    def representationType : SchemeType = valueListType

    def representationTypeOpt : Option[ValueType] = 
      Some(representationType)
    
    def toValueListType : SchemeType =
      valueListType
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
}
