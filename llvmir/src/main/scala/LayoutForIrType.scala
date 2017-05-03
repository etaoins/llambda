package io.llambda.llvmir


final class UnableToDetermineLayoutException(msg: String) extends Exception(msg)

object LayoutForIrType {
  private def alignValue(value: Int, alignment: Int): Int =
    if (alignment == 0) {
      // No alignment
      value
    }
    else if ((value % alignment) == 0) {
      // Already aligned
      value
    }
    else {
      value + (alignment - (value % alignment))
    }


  def apply(dataLayout: DataLayout)(irType: FirstClassType): TypeLayout = irType match {
    case _: PointerType =>
      dataLayout.pointerLayout

    case IntegerType(bits) =>
      val sortedIntegerLayouts = dataLayout.integerLayouts.values.toList.sortBy(_.sizeBits)

      sortedIntegerLayouts.dropWhile(_.sizeBits < bits) match {
        case bestFit :: _ =>
          // Use the alignment of the smallest integer type with sufficient bits
          bestFit.copy(sizeBits=bits)

        case Nil =>
          // Use the alignment of the last integer type
          sortedIntegerLayouts.last.copy(sizeBits=bits)
      }

    case fpType: FloatingPointType =>
      dataLayout.floatLayouts(fpType.bits)

    case ArrayType(elements, innerType) =>
      val innerLayout = LayoutForIrType(dataLayout)(innerType)

      val alignedInnerSizeBits = alignValue(innerLayout.sizeBits, innerLayout.abiAlignmentBits)

      val abiAlignmentBits = Math.max(
        innerLayout.abiAlignmentBits,
        dataLayout.aggregateLayout.abiAlignmentBits
      )

      val preferredAlignmentBits = Math.max(
        innerLayout.preferredAlignmentBits,
        dataLayout.aggregateLayout.preferredAlignmentBits
      )

      TypeLayout(alignedInnerSizeBits * elements, abiAlignmentBits, preferredAlignmentBits)

    case StructureType(Nil) =>
      dataLayout.aggregateLayout

    case StructureType(memberTypes) =>
      val abiAlignmentBits = Math.max(
        memberTypes.map(LayoutForIrType(dataLayout)(_).abiAlignmentBits).max,
        dataLayout.aggregateLayout.abiAlignmentBits
      )

      val preferredAlignmentBits = Math.max(
        memberTypes.map(LayoutForIrType(dataLayout)(_).preferredAlignmentBits).max,
        dataLayout.aggregateLayout.preferredAlignmentBits
      )

      val unalignedSizeBits = memberTypes.foldLeft(0) { case (sizeBits, memberType) =>
        val memberLayout = LayoutForIrType(dataLayout)(memberType)
        alignValue(sizeBits, memberLayout.abiAlignmentBits) + memberLayout.sizeBits
      }

      val sizeBits = alignValue(unalignedSizeBits, abiAlignmentBits)
      TypeLayout(sizeBits, abiAlignmentBits, preferredAlignmentBits)

    case _: UserDefinedType =>
      throw new UnableToDetermineLayoutException("Unable to determine the data layout for user defined types")

    case MetadataType =>
      TypeLayout(0, 0, 0)
  }
}
