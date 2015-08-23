package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

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

    case (EmptySchemeType, _) | (_, EmptySchemeType) =>
      Result()

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

    case (HashMapType(polyKey, polyValue), HashMapType(evidenceKey, evidenceValue)) =>
      visitType(typeVars, polyKey, Nil, evidenceKey, Nil) ++
        visitType(typeVars, polyValue, Nil, evidenceValue, Nil)

    case (ProcedureType(_, _, polyRestOpt, polyReturn), ProcedureType(_, _, evidenceRestOpt, evidenceReturn)) =>
      // Intentionally do not use the formals to resolve polymorphic variables. The formals aren't "evidence" of types
      // as they're passed input from other polymorphic collections. For example, the signature for (filter) might be:
      // (All (A) (-> A <boolean>) (Listof A) (Listof A))
      //
      // If the "A" in the formals for the predicate was used to resolve types it would typically cause A to become
      // <any> for the entire type. However, if it's not used to resolve types it will take on the type of the input
      // list. This adds additional type safety by ensuring the predicate procedure takes the input type.

      val polyReturnListType = polyReturn.toValueListType
      val evidenceReturnListType = evidenceReturn.toValueListType

      visitType(typeVars, polyReturnListType, Nil, evidenceReturnListType, Nil)

    case _ =>
      Result()
  }

  /** Resolves variables in a polymorphic type based on an evidence type
    *
    * For example, given a polymorphic type of (Pairof A <boolean>) and an evidence type of (Pairof <port> <boolean>)
    * this will return {A => <port>}. If a variable occur multiple times literally in the polymorphic type or after
    * recursive expansion then the union type of its occurrences will be used.
    *
    * [[ReconcileTypeVars]] must be called to process this result before it can be used for instantiation.
    *
    * @param  typeVars  Set of type variables in the polymorphic type
    * @param  poly      Polymorphic template type
    * @param  evidence  Evidence type to match against
    */
  def apply(typeVars : Set[TypeVar], poly : ValueType, evidence : SchemeType) : Result = {
    visitType(typeVars, poly, Nil, evidence, Nil)
  }
}
