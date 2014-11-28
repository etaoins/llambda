package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

object ResolveTypeVars {
  case class Result(values : Map[Int, SchemeType] = Map()) {
    def +(newValue : (Int, SchemeType)) : Result = {
      val (index, newType) = newValue

      values.get(index) match {
        case Some(existingType) =>
          val unionedType = existingType + newType
          Result(values + (index -> unionedType))

        case None =>
          Result(values + (index -> newType))
      }
    }

    def ++(other : Result) : Result = {
      other.values.foldLeft(this) { case (intermediateResult, (index, newType)) =>
        intermediateResult + (index -> newType)
      }
    }
  }

  private def visitTypeRef(
      polyRef : SchemeTypeRef,
      polyStack : SchemeType.Stack,
      evidenceRef : SchemeTypeRef,
      evidenceStack : SchemeType.Stack,
      polyDepth : Int
  ) : Result = {
    polyRef match {
      case RecursiveSchemeTypeRef(stackDistance) if stackDistance >= polyDepth =>
        // This is an polymorphic variable!
        val index = stackDistance - polyDepth
        val evidence = evidenceRef.resolve(evidenceStack)

        Result(Map(index -> evidence))

      case RecursiveSchemeTypeRef(innerDistance) =>
        // This is an internal recursive reference in the polymorphic type
        val poly = polyRef.resolve(polyStack)
        val evidence = evidenceRef.resolve(evidenceStack)

        if (polyStack.contains(poly) && evidenceStack.contains(evidence)) {
          // We've recursed - we can't resolve any variables from this
          Result()
        }
        else {
          // This is an upward reference so our poly depth decreases
          visitType(poly, polyStack, evidence, evidenceStack, polyDepth - innerDistance - 1)
        }

      case DirectSchemeTypeRef(poly) =>
        // This is an internal recursive reference in the polymorphic type
        val evidence  = evidenceRef.resolve(evidenceStack)
        visitType(poly, polyStack, evidence, evidenceStack, polyDepth)
    }
  }

  private def visitType(
      poly : ValueType,
      polyStack : SchemeType.Stack,
      evidence : ValueType,
      evidenceStack : SchemeType.Stack,
      polyDepth : Int
  ) : Result = (poly, evidence) match {
    case (polyUnion @ UnionType(polyMembers), _) =>
      val results = polyMembers map { polyMember =>
        visitType(polyMember, polyUnion :: polyStack, evidence, evidenceStack, polyDepth + 1)
      }

      results.reduce(_ ++ _)

    case (_, evidenceUnion @ UnionType(evidenceMembers)) =>
      val results = evidenceMembers map { evidenceMember =>
        visitType(poly, polyStack, evidenceMember, evidenceUnion :: evidenceStack, polyDepth)
      }

      results.reduce(_ ++ _)

    case (polyPair : SpecificPairType, evidencePair : SpecificPairType) =>
      val newPolyStack = polyPair :: polyStack
      val newEvidenceStack = evidencePair :: evidenceStack
      val newPolyDepth = polyDepth + 1

      visitTypeRef(polyPair.carTypeRef, newPolyStack, evidencePair.carTypeRef, newEvidenceStack, newPolyDepth) ++
        visitTypeRef(polyPair.cdrTypeRef, newPolyStack, evidencePair.cdrTypeRef, newEvidenceStack, newPolyDepth)

    case (polyVec @ UniformVectorType(polyRef), evidenceVec @ UniformVectorType(evidenceRef)) =>
      visitTypeRef(polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack, polyDepth + 1)

    case (polyVec @ SpecificVectorType(polyRefs), evidenceVec @ SpecificVectorType(evidenceRefs))
        if polyRefs.size == evidenceRefs.size =>
      val results = (polyRefs zip evidenceRefs) map { case (polyRef, evidenceRef) =>
        visitTypeRef(polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack, polyDepth + 1)
      }

      results.reduce(_ ++ _)

    case (polyVec @ UniformVectorType(polyRef), evidenceVec @ SpecificVectorType(evidenceRefs)) =>
      val results = evidenceRefs map { case evidenceRef =>
        visitTypeRef(polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack, polyDepth + 1)
      }

      results.reduce(_ ++ _)

    case _ =>
      Result()
  }

  /** Resolves variables in a polymorphic type based on an evidence type
    *
    * Polymorphic type variables as represented as SchemeRecursiveTypeRefs that point to outside the polymorphic type.
    * When one of these variables are encountered the corresponding type is taken from the evidence. If the same
    * variable is matched against the evidence multiple times then the union of those types is taken.
    */
  def apply(poly : ValueType, evidence : ValueType) : Result =
    visitType(poly, Nil, evidence, Nil, 0)
}
