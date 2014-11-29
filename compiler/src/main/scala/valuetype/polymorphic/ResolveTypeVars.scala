package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

object ResolveTypeVars {
  case class Result(values : Map[TypeVar, SchemeType] = Map()) {
    def +(newValue : (TypeVar, SchemeType)) : Result = {
      val (typeVar, newType) = newValue

      values.get(typeVar) match {
        case Some(existingType) =>
          val unionedType = existingType + newType
          Result(values + (typeVar -> unionedType))

        case None =>
          Result(values + (typeVar -> newType))
      }
    }

    def ++(other : Result) : Result = {
      other.values.foldLeft(this) { case (intermediateResult, (typeVar, newType)) =>
        intermediateResult + (typeVar -> newType)
      }
    }
  }

  private def visitTypeRef(
      typeVars : Set[TypeVar],
      polyRef : SchemeTypeRef,
      polyStack : SchemeType.Stack,
      evidenceRef : SchemeTypeRef,
      evidenceStack : SchemeType.Stack
  ) : Result = polyRef match {
    case RecursiveSchemeTypeRef(innerDistance) =>
      // This is an internal recursive reference in the polymorphic type
      val poly = polyRef.resolve(polyStack)
      val evidence = evidenceRef.resolve(evidenceStack)

      if (polyStack.contains(poly) && evidenceStack.contains(evidence)) {
        // We've recursed - we can't resolve any variables from this
        Result()
      }
      else {
        visitType(typeVars, poly, polyStack, evidence, evidenceStack)
      }

    case DirectSchemeTypeRef(typeVar : TypeVar) if typeVars.contains(typeVar) =>
      // We found a type variable!
      val evidence  = evidenceRef.resolve(evidenceStack)
      Result(Map(typeVar -> evidence))

    case DirectSchemeTypeRef(poly) =>
      // Visit this type
      val evidence  = evidenceRef.resolve(evidenceStack)
      visitType(typeVars, poly, polyStack, evidence, evidenceStack)
  }

  private def visitType(
      typeVars : Set[TypeVar],
      poly : ValueType,
      polyStack : SchemeType.Stack,
      evidence : SchemeType,
      evidenceStack : SchemeType.Stack
  ) : Result = (poly, evidence) match {
    case (typeVar : TypeVar, evidence) if typeVars.contains(typeVar) =>
      Result(Map(typeVar -> evidence))

    case (polyUnion @ UnionType(polyMembers), _) =>
      val results = polyMembers map { polyMember =>
        visitType(typeVars, polyMember, polyUnion :: polyStack, evidence, evidenceStack)
      }

      results.reduce(_ ++ _)

    case (_, evidenceUnion @ UnionType(evidenceMembers)) =>
      val results = evidenceMembers map { evidenceMember =>
        visitType(typeVars, poly, polyStack, evidenceMember, evidenceUnion :: evidenceStack)
      }

      results.reduce(_ ++ _)

    case (polyPair : SpecificPairType, evidencePair : SpecificPairType) =>
      val newPolyStack = polyPair :: polyStack
      val newEvidenceStack = evidencePair :: evidenceStack

      visitTypeRef(typeVars, polyPair.carTypeRef, newPolyStack, evidencePair.carTypeRef, newEvidenceStack) ++
        visitTypeRef(typeVars, polyPair.cdrTypeRef, newPolyStack, evidencePair.cdrTypeRef, newEvidenceStack)

    case (polyVec @ UniformVectorType(polyRef), evidenceVec @ UniformVectorType(evidenceRef)) =>
      visitTypeRef(typeVars, polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack)

    case (polyVec @ SpecificVectorType(polyRefs), evidenceVec @ SpecificVectorType(evidenceRefs))
        if polyRefs.size == evidenceRefs.size =>
      val results = (polyRefs zip evidenceRefs) map { case (polyRef, evidenceRef) =>
        visitTypeRef(typeVars, polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack)
      }

      results.reduce(_ ++ _)

    case (polyVec @ UniformVectorType(polyRef), evidenceVec @ SpecificVectorType(evidenceRefs)) =>
      val results = evidenceRefs map { case evidenceRef =>
        visitTypeRef(typeVars, polyRef, polyVec :: polyStack, evidenceRef, evidenceVec :: evidenceStack)
      }

      results.reduce(_ ++ _)

    case _ =>
      Result()
  }

  /** Resolves variables in a polymorphic type based on an evidence type */
  def apply(typeVars : Set[TypeVar], poly : ValueType, evidence : SchemeType) : Result = {
    visitType(typeVars, poly, Nil, evidence, Nil)
  }
}
