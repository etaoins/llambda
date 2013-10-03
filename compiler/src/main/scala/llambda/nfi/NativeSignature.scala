package llambda.nfi

trait NativeSignature {
  val fixedArgs : List[NativeType]
  val hasRestArg : Boolean
  val returnType : Option[NativeType]
  val nativeSymbol : String
}

