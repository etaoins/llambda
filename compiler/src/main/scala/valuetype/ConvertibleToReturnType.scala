package io.llambda.compiler.valuetype
import io.llambda

object ConvertibleToReturnType {
  /** Determines if a source return type is convertable to the destination return type
    *
    * This is equivalent to SatisfiesType except all types are convertible to UnitType
    */
  def apply(destType : ReturnType.ReturnType[ValueType], sourceType : ReturnType.ReturnType[ValueType]) : Option[Boolean] =
    (destType, sourceType) match {
      case (ReturnType.Reachable(UnitType), _) =>
        // Everything is convertible to the unit type
        Some(true)

      case (_, ReturnType.Unreachable) =>
        // Everything is convertible from an unreachable value as the conversion will never occur
        Some(true)

      case (destType, sourceType) =>
        SatisfiesType(destType.schemeType, sourceType.schemeType)
    }
}
