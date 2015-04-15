package io.llambda.llvmir.debug

import java.io.File

import io.llambda.llvmir._

case class FilePathMetadata(filename : String, path : String) extends MetadataNode {
  val operandOpts = List(
    Some(MetadataString.fromUtf8String(filename)),
    Some(MetadataString.fromUtf8String(path))
  )
}

object FilePathMetadata {
  def fromFile(file : File) : FilePathMetadata = {
    FilePathMetadata(
      filename=file.getName,
      path=file.getParent
    )
  }
}

