package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.valuetype.ProcedureType

private[planner] object RequiredArityDescription {
  def apply(mandatoryArgs : Int, optionalArgs : Int, hasRestArg : Boolean) : String = {
    if (hasRestArg) {
      s"at least ${mandatoryArgs} arguments"
    }
    else if (optionalArgs == 0) {
      s"exactly ${mandatoryArgs} arguments"
    }
    else {
      s"between ${mandatoryArgs} and ${mandatoryArgs + optionalArgs} arguments"
    }
  }

  def fromProcedureType(procType : ProcedureType) : String = {
    apply(
      mandatoryArgs=procType.mandatoryArgTypes.length,
      optionalArgs=procType.optionalArgTypes.length,
      hasRestArg=procType.restArgMemberTypeOpt.isDefined
    )
  }

  def fromProcedureSignature(signature : ProcedureSignature) : String = {
    apply(
      mandatoryArgs=signature.mandatoryArgTypes.length,
      optionalArgs=signature.optionalArgTypes.length,
      hasRestArg=signature.restArgMemberTypeOpt.isDefined
    )
  }
}
