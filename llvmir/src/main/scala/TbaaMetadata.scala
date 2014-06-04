package io.llambda.llvmir

case class TbaaMetadata(identity : String, parentOpt : Option[Metadata] = None, constant : Boolean = false) extends MetadataNode {
  private val identityMetadata = MetadataString.fromUtf8String(identity)

  val memberOpts = List(
    Some(identityMetadata),
    parentOpt,
    if (constant) Some(IntegerConstant(IntegerType(64), 1)) else None
  ).reverse.dropWhile(!_.isDefined).reverse
}

