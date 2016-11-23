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

    /** Returns the Scheme type of the return values
      *
      * This is useful for type checking or type arithmetic on return types
      */
    def schemeType : SchemeType

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

    def schemeType : SchemeType = valueType.schemeType

    def schemeReturnType : ReturnType[SchemeType] =
      SingleValue(valueType.schemeType)
  }

  /** Represents the return type of a procedure that cannot return
    *
    * This is distinct from the NoReturn ProcedureSignature attribute as its possible for native functions to have
    * non-void return types and also not return.
    */
  object UnreachableValue extends ReturnType[SchemeType] {
    def representationTypeOpt = None

    def schemeType : SchemeType = UnitType

    def schemeReturnType : ReturnType[SchemeType] =
      this
  }
}
