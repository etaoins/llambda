package io.llambda.llvmir

import scala.io.Codec

trait MetadataOperand {
  def toMetadataIr : String
}

/** Represents a metadata value
  *
  * These are used to annotate LLVM IR with arbitrary metadata. They do not represent a concrete value at runtime
  */
sealed abstract class Metadata extends MetadataOperand with Irable {
  def toMetadataIr = toIr
}

case class MetadataString(stringBytes : Seq[Byte]) extends Metadata {
  def toIr : String = {
    "!" + BytesToIrString(stringBytes)
  }
}

object MetadataString {
  def fromUtf8String(str : String) : MetadataString = {
    MetadataString(Codec.toUTF8(str))
  }
}

abstract class MetadataNode extends Metadata {
  val operandOpts : Seq[Option[MetadataOperand]]

  private def operandOptToIr(operandOpt : Option[MetadataOperand]) =
    operandOpt.map(_.toMetadataIr).getOrElse("null")

  def toIr : String = {
    "!{" + operandOpts.map(operandOptToIr).mkString(", ") + "}"
  }

  protected def listToNotNullMetadata(metadataList : List[Metadata]) : Metadata = {
    if (metadataList.isEmpty) {
      // Not sure why this is a thing
      UserDefinedMetadataNode(List(
        Some(IntegerConstant(IntegerType(32), 0))
      ))
    }
    else {
      UserDefinedMetadataNode(metadataList.map(Some(_)))
    }
  }

}

case class UserDefinedMetadataNode(operandOpts : Seq[Option[MetadataOperand]]) extends MetadataNode

case class NumberedMetadata(index : Long) extends Metadata {
  def toIr = "!" + index.toString
}
