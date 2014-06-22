package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}

/** Signature for all boxed procedure values
  *
  * If the underlying procedure has a different signature then a trampoline with this signature will be generated at
  * compile time
  */
object AdaptedProcedureSignature extends ProcedureSignature(
  hasWorldArg=true,
  hasSelfArg=true,
  fixedArgs=Nil,
  hasRestArg=true,
  returnType=Some(vt.AnySchemeType),
  attributes=Set()
)
