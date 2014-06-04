package io.llambda.llvmir

case class MetadataDef(index : Long, metadataNode : MetadataNode) extends Irable  {
  val namedMetadata = NamedMetadata(index) 

  def toIr = namedMetadata.toIr + " = " + metadataNode.toIrWithType
}
