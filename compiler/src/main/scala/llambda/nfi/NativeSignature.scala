package llambda.nfi

trait NativeSignature {
  val hasSelfArg : Boolean
  val fixedArgs : List[NativeType]
  val hasRestArg : Boolean
  val returnType : Option[NativeType]
}
