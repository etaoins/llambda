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
    * @param recordLike        Record-like type to repack
    * @param inlineDataByres   Number of bytes available for inline data storage
    * @param targetPlatform    Target platform to use when determing type sizes and alignments
    */
  def apply(recordLike : vt.RecordLikeType, inlineDataBytes : Int, targetPlatform : TargetPlatform) : PackedRecordLike = {
    // The existing order stored out-of-line
    lazy val existingOutOfLine = PackedRecordLike(recordLike.fields, false)
    // The existing order stored inline
    lazy val existingInline = PackedRecordLike(recordLike.fields, true)

    if (!targetPlatform.usesNaturalAlignment) {
      // We assume natural alignment
      // We have no way of reliably determing sizes so always use out-of-line
      // data
      return existingOutOfLine
    }

    val fieldsWithType = recordLike.fields map { field =>
      field -> recordLike.storageTypeForField(field)
    }

    if (sizeOfStruct(fieldsWithType.map(_._2), targetPlatform) <= inlineDataBytes) {
      // This already fits
      return existingInline
    }

    // Get the sizes of the fields once
    val fieldsWithSize = fieldsWithType.map { case (field, fieldType) =>
      (field, targetPlatform.bytesForType(fieldType))
    }

    // If every size is a power of two we can pack them perfectly in descending order of size
    if (fieldsWithSize.map(_._2).forall(isPowerOfTwo(_)) &&
        (fieldsWithSize.map(_._2).sum <= inlineDataBytes)) {
      val fieldOrder = fieldsWithSize.sortBy(-_._2).map(_._1)

      PackedRecordLike(fieldOrder, true)
    }
    else {
       existingOutOfLine
    }
  }
}

