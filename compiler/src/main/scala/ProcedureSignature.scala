package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

/** Describes the signature of an invokable function
  *
  * This includes both native functions and generated lambdas 
  */
case class ProcedureSignature(
  hasSelfArg : Boolean,
  fixedArgs : List[vt.ValueType],
  hasRestArg : Boolean,
  returnType : Option[vt.ValueType]
)
