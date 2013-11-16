package llambda.nfi

trait NativeSignature {
  val hasClosureArg : Boolean
  val fixedArgs : List[NativeType]
  val hasRestArg : Boolean
  val returnType : Option[NativeType]
}
