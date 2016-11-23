package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ProcedureSignature, ProcedureAttribute}

object SatisfiesSignature {
  private def satisfiesRepresentation(superType : vt.ValueType, derivedType : vt.ValueType) : Boolean = 
    (superType, derivedType) match {
      case (superScheme : vt.SchemeType, derivedScheme : vt.SchemeType) =>
        // All Scheme types are boxed - do normal type derived
        vt.SatisfiesType(superScheme, derivedScheme) == Some(true)

      case (superOther, derivedOther) =>
        // These must be strictly equal
        superOther == derivedOther
    }


  /** Determines if a super signature completely satisfies by a derived signature
    *
    * This means that all invokations of the derived signature will automatically satisfy the super signature. This
    * requires that the signatures:
    *
    * - Have the same calling convention
    * - Both either take world/self arguments or not
    * - Have the same number of mandatory arguments and they're covariant
    * - Have the same number of optional arguments and they're covariant
    * - Have the same number of rest arguments and they're covariant
    *
    * This test is much more strict than procedure type checks which work on an abstract type system level instead of
    * a calling convention level.
    *
    * @param  superSignature    Signature the derived signature must satisfy
    * @param  derivedSignature  Signature to test
    */
  def apply(superSignature : ProcedureSignature, derivedSignature : ProcedureSignature) : Boolean = {
    if (superSignature == derivedSignature) {
      // Fast path without a bunch of potentially expensive type checks
      return true
    }

    if (superSignature.hasWorldArg != derivedSignature.hasWorldArg) {
      return false
    }

    if (superSignature.hasSelfArg != derivedSignature.hasSelfArg) {
      return false
    }

    if (superSignature.mandatoryArgTypes.length != derivedSignature.mandatoryArgTypes.length) {
      return false
    }

    if (superSignature.attributes.contains(ProcedureAttribute.FastCC) !=
        derivedSignature.attributes.contains(ProcedureAttribute.FastCC)) {
      return false
    }

    val mandatoryArgsSatisfy = superSignature.mandatoryArgTypes.zip(derivedSignature.mandatoryArgTypes).forall {
      case (superFixedArg, derivedFixedArg) =>
        // Do a contravariant test
        satisfiesRepresentation(derivedFixedArg, superFixedArg)
    }

    if (!mandatoryArgsSatisfy) {
      return false
    }

    def signatureHasVarArgs(signature : ProcedureSignature) =
      !signature.optionalArgTypes.isEmpty || signature.restArgMemberTypeOpt.isDefined

    if (signatureHasVarArgs(superSignature) != signatureHasVarArgs(derivedSignature)) {
      return false
    }

    val superVarArgsListType = vt.VariableArgsToListType(
      superSignature.optionalArgTypes,
      superSignature.restArgMemberTypeOpt
    )

    val derivedVarArgsListType = vt.VariableArgsToListType(
      derivedSignature.optionalArgTypes,
      derivedSignature.restArgMemberTypeOpt
    )

    if (vt.SatisfiesType(derivedVarArgsListType, superVarArgsListType) != Some(true)) {
      return false
    }

    val returnTypeSatisfies = (superSignature.returnType, derivedSignature.returnType) match {
      case (superReturnType, derivedReturnType)
          if superReturnType.representationTypeOpt.isDefined != derivedReturnType.representationTypeOpt.isDefined =>
        false

      case (vt.ReturnType.SingleValue(superSingle), vt.ReturnType.SingleValue(derivedSingle)) =>
        satisfiesRepresentation(superSingle, derivedSingle)

      case _ =>
        false
    }

    returnTypeSatisfies
  }
}
