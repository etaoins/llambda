package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}

object ApplicableTypeToAdaptedSignature extends (vt.ApplicableType => ProcedureSignature) {
  /** Returns the expected signature for the specified applicable type
    *
    * This is used when creating procedure cells with a specific applicable type
    */
  def apply(applicableType : vt.ApplicableType) : ProcedureSignature =
    applicableType match {
      case procType : vt.ProcedureType =>
        ProcedureSignature(
          hasWorldArg=true,
          hasSelfArg=true,
          fixedArgTypes=procType.fixedArgTypes,
          restArgMemberTypeOpt=procType.restArgMemberTypeOpt,
          returnType=procType.returnType,
          attributes=Set()
        )

      case _ : vt.CaseProcedureType =>
        CaseLambdaSignature
    }
}
