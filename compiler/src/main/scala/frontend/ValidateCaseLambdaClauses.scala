package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{BadSpecialFormException, SourceLocated}
import llambda.compiler.{valuetype => vt}

private[frontend] object ValidateCaseLambdaClauses {
  /** Determines if a (case-lambda)s clauses are semantically valid
    *
    * Thie ensures that no clauses are unconditionally shadowed by the signature of a previous clause
    */
  def apply(clauses : List[(SourceLocated, vt.ProcedureType)]) {
    if (clauses.length > 1) {
      clauses.tail.scanLeft(clauses.head._2) {
        case (prevSignature, (located, signature)) =>
          val prevFixedArgCount = prevSignature.fixedArgTypes.length
          val fixedArgCount = signature.fixedArgTypes.length 

          if (prevSignature.restArgMemberTypeOpt.isDefined) {
            val message = "Case unreachable; previous case had a rest argument"
            throw new BadSpecialFormException(located, message) 
          }
          else if (fixedArgCount < prevFixedArgCount) {
            val message = s"Case unreachable; has ${fixedArgCount} fixed arguments while previous case has ${prevFixedArgCount}"
            throw new BadSpecialFormException(located, message) 
          }
          else if ((prevFixedArgCount == fixedArgCount) && !signature.restArgMemberTypeOpt.isDefined) {
            val message = "Case unreachable; previous case had same fixed number of arguments and this case does not have a rest argument"
            throw new BadSpecialFormException(located, message) 
          }

          signature
      }
    }
  }
}
