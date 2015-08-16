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
    * - Have the same number of fixed arguments and they're covariant
    * - Have the same number of rest arguments and they're covariant
    * - Either both return a single value or both return multiple values with covariant types
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

    if (superSignature.fixedArgTypes.length != derivedSignature.fixedArgTypes.length) {
      return false
    }

    if (superSignature.attributes.contains(ProcedureAttribute.FastCC) !=
        derivedSignature.attributes.contains(ProcedureAttribute.FastCC)) {
      return false
    }

    val fixedArgsSatisfy = superSignature.fixedArgTypes.zip(derivedSignature.fixedArgTypes).forall { 
      case (superFixedArg, derivedFixedArg) =>
        // Do a contravariant test
        satisfiesRepresentation(derivedFixedArg, superFixedArg)
    }

    if (!fixedArgsSatisfy) {
      return false
    }

    val restArgSatifies = (superSignature.restArgMemberTypeOpt, derivedSignature.restArgMemberTypeOpt) match {
      case (Some(superRestArg), Some(derivedRestArg)) =>
        // Do a contravariant test
        vt.SatisfiesType(derivedRestArg, superRestArg) == Some(true)

      case (None, None) =>
        true

      case _ =>
        false
    }

    if (!restArgSatifies) {
      return false
    }

    val returnTypeSatisfies = (superSignature.returnType, derivedSignature.returnType) match {
      case (superReturnType, derivedReturnType)
          if superReturnType.representationTypeOpt.isDefined != derivedReturnType.representationTypeOpt.isDefined =>
        false

      case (vt.ReturnType.SingleValue(superSingle), vt.ReturnType.SingleValue(derivedSingle)) =>
        satisfiesRepresentation(superSingle, derivedSingle)

      case (vt.ReturnType.MultipleValues(superList), vt.ReturnType.MultipleValues(derivedList)) =>
        vt.SatisfiesType(superList, derivedList) == Some(true)

      case _ =>
        false
    }

    returnTypeSatisfies
  }
}
