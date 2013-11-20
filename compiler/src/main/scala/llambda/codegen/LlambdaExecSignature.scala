package llambda.codegen

import llambda.nfi.{NativeSignature, BoxedValue}
import llambda.{boxedtype => bt}

// This is the signature of llambda_exec
object LlambdaExecSignature extends NativeSignature {
  val hasClosureArg = false
  val fixedArgs = Nil
  val hasRestArg = false
  val returnType = None

  val nativeSymbol = "__llambda_exec"
}

