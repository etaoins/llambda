package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.platform.TargetPlatform

object PackRecordLikeInline {
  /** Packed record-like
    *
    * @param fieldOrder  Record fields in their memory order
    * @param inline      True if the record fields will fit within the inline data area
    */
  case class PackedRecordLike(
    fieldOrder : List[vt.RecordField],
    inline : Boolean
  )

  private def isPowerOfTwo(number : Long) =
    (number & (number - 1)) == 0

  private def sizeOfStruct(fields : Seq[vt.ValueType], targetPlatform : TargetPlatform) : Long = 
    fields.foldLeft(0) { case (currentOffset, field) =>
      val fieldSize = targetPlatform.bytesForType(field)

      // Align to the right size
      val startOffset = if ((currentOffset % fieldSize) == 0) {
        // Already aligned
        currentOffset
      }
      else {
        // Align to the next multiple of our field size
        currentOffset + (fieldSize - (currentOffset % fieldSize))
      }

      startOffset + fieldSize
    }

  /** Attempts to reorder a record's fields to fit within an inline data area
    *
    * Whenever possible existing field order is preserved
    *
    * @param recordFields      Record fields to repack
    * @param inlineDataByres   Number of bytes available for inline data storage
    * @param targetPlatform    Target platform to use when determing type sizes and alignments 
    */
  def apply(recordFields : List[vt.RecordField], inlineDataBytes : Int, targetPlatform : TargetPlatform) : PackedRecordLike = {
    // The existing order stored out-of-line
    lazy val existingOutOfLine = PackedRecordLike(recordFields, false)
    // The existing order stored inline
    lazy val existingInline = PackedRecordLike(recordFields, true)

    if (!targetPlatform.usesNaturalAlignment) {
      // We assume natural alignment
      // We have no way of reliably determing sizes so always use out-of-line
      // data
      return existingOutOfLine
    }

    if (sizeOfStruct(recordFields.map(_.fieldType), targetPlatform) <= inlineDataBytes) {
      // This already fits
      return existingInline
    }

    // Get the sizes of the fields once
    val fieldsWithSizes = recordFields.map { field =>
      (field, targetPlatform.bytesForType(field.fieldType))
    }

    // Make sure all of the field sizes are powers of two
    // See the comments below
    if (fieldsWithSizes.map(_._2).forall(isPowerOfTwo(_))) {
      // Sort the fields by size in descending order
      // If every field is naturally aligned and its size is a power of two then
      // this will pack the values in to an optimal size
      val candidateOrder = recordFields.sortBy { field =>
        -targetPlatform.bytesForType(field.fieldType)
      }

      // Does the new order fit?
      if (sizeOfStruct(candidateOrder.map(_.fieldType), targetPlatform) <= inlineDataBytes) {
        // Repack successful!
        return PackedRecordLike(candidateOrder, true)
      }
    }

    return existingOutOfLine
  }
}
