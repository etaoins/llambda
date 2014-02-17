package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

// All boxed procedures have this signature
// If they are wrapping a procedure with a different signature a thunk must
// be generated with this signature
object AdaptedProcedureSignature extends ProcedureSignature(
  hasWorldArg=true,
  hasSelfArg=true,
  fixedArgs=Nil,
  hasRestArg=true,
  returnType=Some(vt.IntrinsicCellType(ct.DatumCell))
)
