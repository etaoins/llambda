package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.dialect

object StabiliseType {
  /** Returns the part of a Scheme type that can't be mutated at runtime
    *
    * For example, if pairs are mutable then it's not possible for a storage location to have a stable specific pair
    * type. This function will convert any pairs encountered to the <pair> top type.
    */
  def apply(schemeType : SchemeType, schemeDialect : dialect.Dialect) : SchemeType = schemeType match {
    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(apply(_, schemeDialect))
      SchemeType.fromTypeUnion(replacedMembers)

    case pairType : SpecificPairType if !schemeDialect.pairsAreImmutable =>
      AnyPairType

    case other =>
      other
  }
}
