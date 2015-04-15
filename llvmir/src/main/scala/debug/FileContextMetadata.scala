package io.llambda.llvmir.debug

import io.llambda.llvmir._

case class FileContextMetadata(
  sourcePath : Metadata
) extends MetadataNode {
  val operandOpts = List(
    Some(IntegerConstant(IntegerType(32), 786473)),
    Some(sourcePath)
  )
}
