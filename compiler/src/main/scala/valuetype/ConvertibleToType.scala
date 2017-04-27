package io.llambda.compiler.valuetype


object ConvertibleToType {
  /** Determines if a source type is convertable to the destination type
    *
    * This is equivalent to SatisfiesType except all types are convertible to UnitType
    */
  def apply(destType: SchemeType, sourceType: SchemeType): Option[Boolean] = (destType, sourceType) match {
    case (UnitType, _) =>
      // Everything is convertible to the unit type
      Some(true)

    case (destType, sourceType) =>
      SatisfiesType(destType, sourceType)
  }
}
