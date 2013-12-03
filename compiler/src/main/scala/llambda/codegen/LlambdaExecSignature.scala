package llambda.codegen

import llambda.ProcedureSignature
import llambda.{celltype => ct}

// This is the signature of llambda_exec
object LlambdaExecSignature extends ProcedureSignature {
  val hasClosureArg = false
  val fixedArgs = Nil
  val hasRestArg = false
  val returnType = None

  val nativeSymbol = "__llambda_exec"
}

