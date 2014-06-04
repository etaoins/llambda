package io.llambda.llvmir

import scala.annotation.tailrec

/** Represents LLVM IR value range metadata
  *
  * These are a series of [start, end) rangs of the possible values an integer
  * loaded from memory. See the LLVM documentation for more information. 
  */
case class RangeMetadata(integerType : IntegerType, ranges : (Long, Long)*) extends MetadataNode {
  val memberOpts = ranges flatMap { case (start, end) =>
    List(
      Some(IntegerConstant(integerType, start)),
      Some(IntegerConstant(integerType, end))
    )
  }
}

object RangeMetadata {
  @tailrec
  private def rangesFromSortedValues(sortedValues : List[Long], acc : List[(Long, Long)]) : List[(Long, Long)] = sortedValues match {
    case Nil =>
      acc.reverse

    case minimumValue :: tailValues =>
      // Drop all the values sequentially following this one
      // We can't use Stream.from here because it uses Int
      val newTailValues = (tailValues.zipWithIndex.dropWhile { case (tailValue, index) =>
        tailValue == (minimumValue + index + 1)
      }).map(_._1)

      val numValuesInRange = tailValues.length - newTailValues.length + 1 
      val endValueExclusive = minimumValue + numValuesInRange

      rangesFromSortedValues(newTailValues, (minimumValue, endValueExclusive) :: acc)
  }

  /** Creates range metadata from a list of possible values */
  def fromPossibleValues(integerType : IntegerType, possibleValues : TraversableOnce[Long]) : RangeMetadata = {
    // Remove duplicates and sort
    val sortedValues = possibleValues.toSet.toList.sorted
    RangeMetadata(integerType, rangesFromSortedValues(sortedValues, Nil) : _*)
  }
}
