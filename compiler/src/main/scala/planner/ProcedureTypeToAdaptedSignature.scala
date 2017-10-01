package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.codegen.CompactRepresentationForType

object ProcedureTypeToAdaptedSignature extends (vt.ProcedureType => ProcedureSignature) {
  /** Returns the expected signature for the specified procedure type
    *
    * This is used when creating procedure cells with a specific procedure
    */
  def apply(procType: vt.ProcedureType): ProcedureSignature = {
    val compactReturnType = procType.returnType match {
      case vt.ReturnType.Reachable(schemeType) =>
        vt.ReturnType.Reachable(CompactRepresentationForType(schemeType))

      case other =>
        other
    }

    ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      mandatoryArgTypes=procType.mandatoryArgTypes.map(CompactRepresentationForType),
      optionalArgTypes=procType.optionalArgTypes,
      restArgMemberTypeOpt=procType.restArgMemberTypeOpt,
      returnType=compactReturnType,
      attributes=Set()
    )
  }
}
