package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.platform.TargetPlatform


private[codegen] object PackRecordLikeInline {
  /** Packed record-like
    *
    * @param fieldOrder  Record fields in their memory order
    * @param inline      True if the record fields will fit within the inline data area
    */
  case class PackedRecordLike(
    fieldOrder: List[vt.RecordField],
    inline: Boolean
  )

  private def sizeOfStruct(fields: List[vt.ValueType], targetPlatform: TargetPlatform): Long = {
    val llvmType = StructureType(
      fields.map(ValueTypeToIr).map(_.irType)
    )

    LayoutForIrType(targetPlatform.dataLayout)(llvmType).sizeBits / 8
  }

  /** Reorders a record's fields to take up a minimum size
    *
    * @param recordLike        Record-like type to repack
    * @param inlineDataByres   Number of bytes available for inline data storage
    * @param targetPlatform    Target platform to use when determing type sizes and alignments
    */
  def apply(recordLike: vt.RecordLikeType, inlineDataBytes: Int, targetPlatform: TargetPlatform): PackedRecordLike = {
    val sortedFieldWithTypeAndSize = recordLike.fields.map({ case field =>
      val fieldType = recordLike.typeForField(field)
      val sizeBits = LayoutForIrType(targetPlatform.dataLayout)(ValueTypeToIr(fieldType).irType).sizeBits

      (field, fieldType, sizeBits)
    }).sortBy(-_._3)

    val recordDataBytes = sizeOfStruct(sortedFieldWithTypeAndSize.map(_._2), targetPlatform)
    val isInline = recordDataBytes <= inlineDataBytes

    PackedRecordLike(sortedFieldWithTypeAndSize.map(_._1), isInline)
  }
}

