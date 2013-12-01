package llambda.codegen

import llambda.nfi.NativeSignature
import llambda.{valuetype => vt}
import llambda.{boxedtype => bt}

// All boxed procedures have this signature
// If they are wrapping a function with a different signature a thunk must
// be generated
object BoxedProcedureSignature extends NativeSignature {
  val hasClosureArg = true 
  val fixedArgs = Nil
  val hasRestArg = true
  val returnType = Some(vt.BoxedIntrinsicType(bt.BoxedDatum))
}
