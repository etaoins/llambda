package llambda.codegen

import llambda.nfi.{NativeSignature, BoxedValue}
import llambda.{boxedtype => bt}

// All boxed procedures have this signature
// If they are wrapping a function with a different signature a thunk must
// be generated
object BoxedProcedureSignature extends NativeSignature {
  val hasSelfArg = true 
  val fixedArgs = Nil
  val hasRestArg = true
  val returnType = Some(BoxedValue(bt.BoxedDatum))
}
