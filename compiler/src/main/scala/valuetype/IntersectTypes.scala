package io.llambda.compiler.valuetype
import io.llambda

object IntersectTypes {
  private[valuetype] def intersectTypeSets(
     leftTypes : Set[NonUnionSchemeType],
     leftUnionOpt : Option[UnionType],
     rightTypes : Set[NonUnionSchemeType],
     rightUnionOpt : Option[UnionType]
  ) : SchemeType = {
    val leftTypeStack = leftUnionOpt.toList
    val rightTypeStack = rightUnionOpt.toList

    // These are the type members from the left that are more derived than the ones on the right
    val moreDerivedLeftTypes = leftTypes.filter { leftType =>
      rightTypes.exists { rightType =>
        SatisfiesType.stackedSatisfiesType(rightType :: rightTypeStack, leftType :: leftTypeStack) == Some(true)
      }
    }
    
    val moreDerivedRightTypes = rightTypes.filter { rightType =>
      leftTypes.exists { leftType =>
        SatisfiesType.stackedSatisfiesType(leftType :: leftTypeStack, rightType :: rightTypeStack) == Some(true)
      }
    }

    val mergedTypes = moreDerivedLeftTypes ++ moreDerivedRightTypes

    if ((leftTypes == mergedTypes) || (rightTypes == mergedTypes)) {
      // No need to unroll the types
      return SchemeType.fromTypeUnion(mergedTypes)
    }

    val unrolledLeftTypes = leftUnionOpt match {
      case Some(leftUnion) =>
        moreDerivedLeftTypes.map(leftUnion.unrollChildType)

      case _ =>
        moreDerivedLeftTypes
    }
    
    val unrolledRightTypes = rightUnionOpt match {
      case Some(rightUnion) =>
        moreDerivedRightTypes.map(rightUnion.unrollChildType)

      case _ =>
        moreDerivedRightTypes
    }

    SchemeType.fromTypeUnion(unrolledLeftTypes ++ unrolledRightTypes)
  }

  /** Returns the intersection of two types
    *
    * The intersection of two types is the type that both the passed types satisfy. For example, the  intersection of 
    * (U <symbol> <string>) and (U <symbol> <boolean>) is <symbol>.
    */
  def apply(type1 : SchemeType, type2 : SchemeType) : SchemeType = {
    def expandType(schemeType : SchemeType) : (Set[NonUnionSchemeType], Option[UnionType]) = schemeType match {
      case unionType @ UnionType(memberTypes) =>
        (memberTypes, Some(unionType))

      case nonUnionType : NonUnionSchemeType =>
        (Set(nonUnionType), None)
    }

    val (type1Members, type1UnionOpt) = expandType(type1)
    val (type2Members, type2UnionOpt) = expandType(type2)

    intersectTypeSets(type1Members, type1UnionOpt, type2Members, type2UnionOpt)
  }
}
