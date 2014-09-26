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
        // Case procedures use the TopProcedureType signature for two reasons
        // - Calculating a signature that covers all signatures is complex
        // - The signature will likely not be much more efficient than the TopProcedureType (it almost certainly
        //   requires a rest argument) and this will avoid creating an adapter procedure if it is cast to the 
        //   TopProcedureType later
        ProcedureSignature(
          hasWorldArg=true,
          hasSelfArg=true,
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues,
          attributes=Set()
        )
    }
}
