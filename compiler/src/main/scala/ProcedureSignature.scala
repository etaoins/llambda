package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

trait ProcedureSignature {
  val hasSelfArg : Boolean
  val fixedArgs : List[vt.ValueType]
  val hasRestArg : Boolean
  val returnType : Option[vt.ValueType]
}
