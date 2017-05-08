package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.platform.TargetPlatform


private[codegen] object PackRecordLike {
  /** Packed record-like
    *
    * @param fieldOrder  Record fields in their memory order
    * @param sizeBytes   Size of the record data
    */
  case class PackedRecordLike(
    fieldOrder: List[vt.RecordField],
    sizeBytes: Int
  )

  /** Reorders a record's fields to take up a minimum size
    *
    * @param parentStructTypeOpt  LLVM IR structure of the parent record-like
    * @param recordLike           Record-like type to repack
    * @param targetPlatform       Target platform to use when determing type sizes and alignments
    */
  def apply(
    parentStructTypeOpt: Option[StructureType],
    recordLike: vt.RecordLikeType,
    targetPlatform: TargetPlatform
  ): PackedRecordLike = {
    val sortedFieldWithTypeAndSize = recordLike.fields.map({ case field =>
      val fieldType = recordLike.typeForField(field)
      val sizeBits = LayoutForIrType(targetPlatform.dataLayout)(ValueTypeToIr(fieldType).irType).sizeBits

      (field, fieldType, sizeBits)
    }).sortBy(-_._3)

    // We can't reuse sizeBits here because it does not take in to account alignment. Build a full struct instead.
    val structType = StructureType(
      parentStructTypeOpt.toList ++
      sortedFieldWithTypeAndSize.map { case (_, valueType, _) => ValueTypeToIr(valueType).irType }
    )

    val recordDataBytes = LayoutForIrType(targetPlatform.dataLayout)(structType).sizeBits / 8

    PackedRecordLike(sortedFieldWithTypeAndSize.map(_._1), recordDataBytes)
  }
}

