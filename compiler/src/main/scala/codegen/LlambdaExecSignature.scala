package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{celltype => ct}

// This is the signature of llambda_exec
object LlambdaExecSignature extends ProcedureSignature(
  hasWorldArg=true,
  hasSelfArg=false,
  fixedArgs=Nil,
  hasRestArg=false,
  returnType=None
) {
  val nativeSymbol = "__llambda_exec"
}
