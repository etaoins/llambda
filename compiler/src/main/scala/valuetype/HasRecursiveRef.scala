package io.llambda.compiler.valuetype
import io.llambda

object HasRecursiveRef {
  private def refHasRecursiveRef(schemeTypeRef : SchemeTypeRef, depth : Int) : Boolean = schemeTypeRef match {
    case RecursiveSchemeTypeRef(refDepth) =>
      refDepth == depth

    case DirectSchemeTypeRef(directType) =>
      typeHasRecursiveRef(directType, depth + 1)
  }

  private def typeHasRecursiveRef(schemeType : SchemeType, depth : Int) : Boolean = schemeType match {
    case UnionType(memberTypes) =>
      memberTypes.exists(typeHasRecursiveRef(_, depth + 1))

    case pairType : PairType =>
      refHasRecursiveRef(pairType.carTypeRef, depth) ||
        refHasRecursiveRef(pairType.cdrTypeRef, depth)

    case SpecificVectorType(memberTypeRefs) =>
      memberTypeRefs.exists { memberTypeRef =>
        refHasRecursiveRef(memberTypeRef, depth)
      }

    case UniformVectorType(memberTypeRef)  =>
      refHasRecursiveRef(memberTypeRef, depth)

    case applicableType : ApplicableType =>
      // Procedure types explicitly have recursion disabled at the moment
      false

    case _ : SchemeTypeAtom | _ : LiteralBooleanType | _ : LiteralSymbolType | _ : RecordType =>
      false
  }
  
  /** Checks if the passed type has any recursive references to itself from within a child type */
  def apply(schemeType : SchemeType) : Boolean = 
    typeHasRecursiveRef(schemeType, 0)
}
