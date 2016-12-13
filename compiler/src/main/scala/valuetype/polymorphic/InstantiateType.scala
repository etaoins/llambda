package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

object InstantiateType {
  private def visitTypeRef(
      typeVars: ReconcileTypeVars.Result,
      polyRef: SchemeTypeRef
  ): SchemeTypeRef = polyRef match {
    case recursiveRef: RecursiveSchemeTypeRef =>
      recursiveRef

    case DirectSchemeTypeRef(typeVar: TypeVar) =>
      DirectSchemeTypeRef(typeVars.values(typeVar))

    case DirectSchemeTypeRef(directType) =>
      DirectSchemeTypeRef(
        apply(typeVars, directType)
      )
  }

  private def visitProcedureType(
      typeVars: ReconcileTypeVars.Result,
      procType: ProcedureType
  ): ProcedureType = procType match {
    case ProcedureType(mandatoryArgTypes, optionalArgTypes, restArgMemberTypeOpt, returnType) =>
      ProcedureType(
        mandatoryArgTypes=mandatoryArgTypes.map(apply(typeVars, _)),
        optionalArgTypes=optionalArgTypes.map(apply(typeVars, _)),
        restArgMemberTypeOpt=restArgMemberTypeOpt.map(apply(typeVars, _)),
        returnType=instantiateReturnType(typeVars, returnType)
      )
  }

  def instantiateReturnType[T >: SchemeType <: ValueType](
      typeVars: ReconcileTypeVars.Result,
      returnType: ReturnType.ReturnType[T]
  ): ReturnType.ReturnType[T] = returnType match {
    case ReturnType.Unreachable =>
      ReturnType.Unreachable

    case ReturnType.Reachable(valueType) =>
      ReturnType.Reachable(apply(typeVars, valueType))
  }

  /** Instantiates a polymorphic type based on reconciled type variables
    *
    * This replaces all occurrences of the type variables inside the polymorphic template with the corresponding
    * reconciled type in typeVars
    *
    * @param  typeVars  Reconciled type variables as returned by ReconcileTypeVars
    * @param  poly      Polymorphic template type
    * @return Expanded template type
    */
  def apply[T >: SchemeType <: ValueType](typeVars: ReconcileTypeVars.Result, poly: T): T = poly match {
    case _ if typeVars.values.isEmpty =>
      // Nothing to do!
      poly

    case typeVar: TypeVar =>
      typeVars.values(typeVar)

    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(apply(typeVars, _))
      SchemeType.fromTypeUnion(replacedMembers)

    case pairType: SpecificPairType =>
      val replacedCar = visitTypeRef(typeVars, pairType.carTypeRef)
      val replacedCdr = visitTypeRef(typeVars, pairType.cdrTypeRef)

      SpecificPairType(replacedCar, replacedCdr)

    case procType: ProcedureType =>
      visitProcedureType(typeVars, procType)

    case CaseProcedureType(clauseTypes) =>
      CaseProcedureType(clauseTypes.map(visitProcedureType(typeVars, _)))

    case recordType: RecordType =>
      // Record types cannot have unresovled type variables inside of them - they must be fully instantiated when
      // they're bound to a type
      recordType

    case HashMapType(keyType, valueType) =>
      HashMapType(apply(typeVars, keyType), apply(typeVars, valueType))

    case _: LeafType =>
      poly
  }
}
