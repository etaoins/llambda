package llambda.nfi

import llambda.{valuetype => vt}

trait NativeSignature {
  val hasClosureArg : Boolean
  val fixedArgs : List[vt.ValueType]
  val hasRestArg : Boolean
  val returnType : Option[vt.ValueType]
}
