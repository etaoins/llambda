package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{ProcedureSignature, ReturnType}
import llambda.compiler.{valuetype => vt}

// This is the signature of __llambda_top_level
object LlambdaTopLevelSignature extends ProcedureSignature(
  hasWorldArg=true,
  hasSelfArg=false,
  fixedArgs=Nil,
  restArgOpt=None,
  returnType=ReturnType.SingleValue(vt.UnitType),
  attributes=Set()
) {
  val nativeSymbol = "__llambda_top_level"
}
