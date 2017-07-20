package io.llambda.compiler.celltype
import io.llambda

import llambda.compiler.platform.TargetPlatform


private object MaxByteAddressibleLength {
  def apply(platform: TargetPlatform): Long = platform.dataLayout.pointerLayout.sizeBits match {
    case largeValue if largeValue >= 63 =>
      // We used signed 64bit integers so we cannot express lengths larger that 2^63-1
      Long.MaxValue

    case otherBits =>
      1L << otherBits
  }
}

object GarbageState {
  val HeapAllocatedCell = 0
  val GlobalConstant = 1
  val StackAllocatedCell = 2
}

object VectorCellConstants {
  def minPossibleLength: Long = 0L

  /** Returns the maxium possible size of a vector
    *
    * We can constrain the length of a vector by assuming that we must always be able to create a pointer to an element
    * in the vector.
    */
  def maxPossibleLength(platform: TargetPlatform): Long = {
    // Be careful to avoid overflowing a Long on 64 bit platforms
    // Calculate the number of bits for a pointer and then "discount" the bytes required to store a pointer
    val pointerBits = platform.dataLayout.pointerLayout.sizeBits
    val pointerStorageDiscountBits = 31 - Integer.numberOfLeadingZeros(pointerBits / 8)

    1L << (pointerBits - pointerStorageDiscountBits)
  }
}

object BytevectorCellConstants {
  def minPossibleLength: Long = 0L

  def maxPossibleLength = MaxByteAddressibleLength.apply _
}

object SymbolCellConstants {
  /** Value that inlineByteLength takes when the symbol is stored on the heap */
  val heapSymbolInlineByteLength = 255

  /** Maximum number of bytes that an can be stored in an inline symbol */
  lazy val maxInlineSymbolBytes = InlineSymbolCell.inlineDataIrType.elements
}

object StringCellConstants {
  /** Value that inlineByteLength takes when the string is stored on the heap */
  val heapStringInlineByteLength = 255

  /** Maximum number of bytes that an can be stored in an inline string */
  lazy val maxInlineStringBytes = InlineStringCell.inlineDataIrType.elements
}
