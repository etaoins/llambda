package io.llambda.llvmir

case class NumberedMetadataDef(index: Long, metadataNode: MetadataNode) extends Irable {
  val numberedMetadata = NumberedMetadata(index)

  def toIr = numberedMetadata.toIr + " = " + metadataNode.toIr
}
