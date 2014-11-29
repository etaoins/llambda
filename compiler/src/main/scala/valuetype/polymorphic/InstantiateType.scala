package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

object InstantiateType {
  private def visitTypeRef(
      typeVars : ReconcileTypeVars.Result,
      polyRef : SchemeTypeRef
  ) : SchemeTypeRef = polyRef match {
    case recursiveRef : RecursiveSchemeTypeRef =>
      recursiveRef

    case DirectSchemeTypeRef(typeVar : TypeVar) =>
      DirectSchemeTypeRef(typeVars.values(typeVar))

    case DirectSchemeTypeRef(directType) =>
      DirectSchemeTypeRef(
        apply(typeVars, directType)
      )
  }

  private def visitProcedureType(
      typeVars : ReconcileTypeVars.Result,
      procType : ProcedureType
  ) : ProcedureType = procType match {
    case ProcedureType(fixedArgTypes, restArgMemberTypeOpt, returnType) =>
      ProcedureType(
        fixedArgTypes=fixedArgTypes.map(apply(typeVars, _)),
        restArgMemberTypeOpt=restArgMemberTypeOpt.map(apply(typeVars, _)),
        returnType=instantiateReturnType(typeVars, returnType)
      )
  }

  def instantiateReturnType(
      typeVars : ReconcileTypeVars.Result,
      returnType : ReturnType.ReturnType
  ) : ReturnType.ReturnType = returnType match {
    case ReturnType.SingleValue(valueType) =>
      ReturnType.SingleValue(apply(typeVars, valueType))

    case ReturnType.MultipleValues(valueList) =>
      ReturnType.MultipleValues(apply(typeVars, valueList))
  }

  def apply[T >: SchemeType <: ValueType](typeVars : ReconcileTypeVars.Result, poly : T) : T = poly match {
    case _ if typeVars.values.isEmpty =>
      // Nothing to do!
      poly

    case typeVar : TypeVar =>
      typeVars.values(typeVar)

    case UnionType(memberTypes) =>
      val replacedMembers = memberTypes.map(apply(typeVars, _))
      SchemeType.fromTypeUnion(replacedMembers)

    case pairType : SpecificPairType =>
      val replacedCar = visitTypeRef(typeVars, pairType.carTypeRef)
      val replacedCdr = visitTypeRef(typeVars, pairType.cdrTypeRef)

      SpecificPairType(replacedCar, replacedCdr)

    case SpecificVectorType(memberTypeRefs) =>
      val replacedMemberTypeRefs = memberTypeRefs map { memberTypeRef =>
        visitTypeRef(typeVars, memberTypeRef)
      }

      SpecificVectorType(replacedMemberTypeRefs)

    case UniformVectorType(memberTypeRef)  =>
      val replacedMemberType = visitTypeRef(typeVars, memberTypeRef)

      VectorOfType(replacedMemberType)

    case procType : ProcedureType =>
      visitProcedureType(typeVars, procType)

    case CaseProcedureType(clauseTypes) =>
      CaseProcedureType(clauseTypes.map(visitProcedureType(typeVars, _)))

    case _ : LeafType =>
      poly
  }
}
