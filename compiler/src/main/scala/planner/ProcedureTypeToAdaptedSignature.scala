package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}

object ProcedureTypeToAdaptedSignature extends (vt.ProcedureType => ProcedureSignature) {
  /** Returns the expected signature for the specified procedure type
    *
    * This is used when creating procedure cells with a specific procedure type
    */
  def apply(procType : vt.ProcedureType) : ProcedureSignature = 
    ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgTypes=procType.fixedArgTypes,
      restArgMemberTypeOpt=procType.restArgMemberTypeOpt,
      returnType=procType.returnType,
      attributes=Set()
    )
}
