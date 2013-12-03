package llambda.codegen

import llambda.ProcedureSignature
import llambda.{valuetype => vt}
import llambda.{celltype => ct}

// All boxed procedures have this signature
// If they are wrapping a procedure with a different signature a thunk must
// be generated with this signature
object AdaptedProcedureSignature extends ProcedureSignature {
  val hasClosureArg = true 
  val fixedArgs = Nil
  val hasRestArg = true
  val returnType = Some(vt.IntrinsicCellType(ct.DatumCell))
}
