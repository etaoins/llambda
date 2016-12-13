package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{BadSpecialFormException, SourceLocated}
import llambda.compiler.{valuetype => vt}

private[frontend] object ValidateCaseLambdaClauses {
  /** Determines if a (case-lambda)s clauses are semantically valid
    *
    * Thie ensures that no clauses are unconditionally shadowed by the signature of a previous clause
    */
  def apply(clauses: List[(SourceLocated, vt.ProcedureType)]) {
    for ((located, clause) <- clauses) {
      if (clause.optionalArgTypes.length > 0) {
        throw new BadSpecialFormException(located, "Optional arguments cannot be used with case lambdas")
      }
    }

    if (clauses.length > 1) {
      clauses.tail.scanLeft(clauses.head._2) {
        case (prevSignature, (located, signature)) =>
          val prevMandatoryArgCount = prevSignature.mandatoryArgTypes.length
          val mandatoryArgCount = signature.mandatoryArgTypes.length

          if (prevSignature.restArgMemberTypeOpt.isDefined) {
            val message = "Case unreachable; previous case had a rest argument"
            throw new BadSpecialFormException(located, message)
          }
          else if ((mandatoryArgCount <= prevMandatoryArgCount) && !signature.restArgMemberTypeOpt.isDefined) {
            val message = s"Case unreachable; has ${mandatoryArgCount} fixed arguments while previous case has ${prevMandatoryArgCount}"
            throw new BadSpecialFormException(located, message)
          }

          signature
      }
    }
  }
}
