package io.llambda.compiler.valuetype
import io.llambda

object HasRecursiveRef {
  private def refHasRecursiveRef(schemeTypeRef: SchemeTypeRef, depth: Int): Boolean = schemeTypeRef match {
    case RecursiveSchemeTypeRef(refDepth) =>
      refDepth == depth

    case DirectSchemeTypeRef(directType) =>
      typeHasRecursiveRef(directType, depth + 1)
  }

  private def typeHasRecursiveRef(schemeType: SchemeType, depth: Int): Boolean = schemeType match {
    case UnionType(memberTypes) =>
      memberTypes.exists(typeHasRecursiveRef(_, depth + 1))

    case pairType: PairType =>
      refHasRecursiveRef(pairType.carTypeRef, depth) ||
        refHasRecursiveRef(pairType.cdrTypeRef, depth)

    case _: NonRecursiveType =>
      false
  }

  /** Checks if the passed type has any recursive references to itself from within a child type */
  def apply(schemeType: SchemeType): Boolean =
    typeHasRecursiveRef(schemeType, 0)
}
