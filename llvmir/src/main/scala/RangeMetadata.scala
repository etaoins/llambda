package io.llambda.llvmir

import scala.annotation.tailrec

/** Represents LLVM IR value range metadata
  *
  * These are a series of [start, end) ranges of the possible values an integer loaded from memory. See the LLVM
  * documentation for more information.
  */
case class RangeMetadata(integerType : IntegerType, ranges : (Long, Long)*) extends MetadataNode {
  val operandOpts = ranges flatMap { case (start, end) =>
    List(
      Some(IntegerConstant(integerType, start)),
      Some(IntegerConstant(integerType, end))
    )
  }
}

object RangeMetadata {
  @tailrec
  private def rangesFromSortedValues(
      intBits : Int,
      sortedValues : List[Long],
      acc : List[(Long, Long)]
  ) : List[(Long, Long)] = sortedValues match {
    case Nil =>
      acc.reverse

    case minimumValue :: tailValues =>
      val maxIntValue = 2L << (intBits - 1)

      // Drop all the values sequentially following this one
      val newTailValues = (tailValues.zipWithIndex.dropWhile { case (tailValue, index) =>
        tailValue == (minimumValue + index + 1)
      }).map(_._1)

      val numValuesInRange = tailValues.length - newTailValues.length + 1
      // Make sure we wrap the value back to 0 if it's at the end of the range
      val endValueExclusive = (minimumValue + numValuesInRange) % (2L << (intBits - 1))

      rangesFromSortedValues(intBits, newTailValues, (minimumValue, endValueExclusive) :: acc)
  }

  /** Joins the first and last range together if they are continugous modulo the integer size
    *
    * For example, (0, 2) (5, 10) (255, 0) will become (255, 2) (5, 10). This is the exact form required by LLVM.
    */
  private def joinWrappedRanges(ranges : List[(Long, Long)]) : Option[List[(Long, Long)]] = {
    val firstRange = ranges.head
    val lastRange = ranges.last

    if ((firstRange._1 == 0) && (lastRange._2 == 0)) {
      if (ranges.length == 1) {
        // We contain all values!
        None
      }
      else {
        Some(
          (lastRange._1, firstRange._2) :: ranges.drop(1).dropRight(1)
        )
      }
    }
    else {
      Some(ranges)
    }
  }

  /** Creates range metadata from a list of possible values
    *
    * If all possible values for the type are passed then None will be returned indicating range metadata should not be
    * emitted
    */
  def fromPossibleValues(integerType : IntegerType, possibleValues : TraversableOnce[Long]) : Option[RangeMetadata] = {
    // Remove duplicates and sort
    val sortedValues = possibleValues.toSet.toList.sorted

    if (sortedValues.isEmpty) {
      throw new InconsistentIrException("Ranges must contain at least one possible value")
    }

    val ranges = rangesFromSortedValues(integerType.bits, sortedValues, Nil)

    val joinedRangesOpt = joinWrappedRanges(ranges)

    joinedRangesOpt map { joinedRanges =>
      RangeMetadata(integerType, joinedRanges.sortBy(_._2) : _*)
    }
  }
}
