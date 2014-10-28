package io.llambda.compiler.valuetype
import io.llambda

object ConvertibleToReturnType {
  /** Determines if a source return type is convertable to the destination return type
    *
    * This is equivalent to SatisfiesType except all types are convertible to UnitType
    */
  def apply(destType : ReturnType.ReturnType, sourceType : ReturnType.ReturnType) : Option[Boolean] =
    (destType, sourceType) match {
      case (ReturnType.SingleValue(UnitType), _) =>
        // Everything is convertible to the unit type
        Some(true)

      case (destType, sourceType) =>
        SatisfiesType(destType.toValueListType, sourceType.toValueListType)
    }
}
