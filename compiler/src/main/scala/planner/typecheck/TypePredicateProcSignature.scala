package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import llambda.compiler.{valuetype => vt}

object TypePredicateProcSignature extends ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    // We must be able to take any data type without erroring out
    mandatoryArgTypes=List(vt.AnySchemeType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.Predicate),
    attributes=Set(ProcedureAttribute.FastCC, ProcedureAttribute.NoCapture)
)
