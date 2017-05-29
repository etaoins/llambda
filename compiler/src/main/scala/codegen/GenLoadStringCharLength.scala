package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}


object GenLoadStringCharLength {
  def apply(initialState: GenerationState)(stringIr: IrValue): (GenerationState, IrValue) = {
    val entryBlock = initialState.currentBlock
    val irFunction = entryBlock.function

    // Determine the possible values of the inline byte length
    val inlineByteLengthPossibleValues = ct.StringCellConstants.heapStringInlineByteLength.toLong ::
      (0L to ct.StringCellConstants.maxInlineStringBytes.toLong).toList

    val inlineByteLengthRangesOpt = RangeMetadata.fromPossibleValues(
      ct.StringCell.inlineByteLengthIrType,
      inlineByteLengthPossibleValues
    )

    val inlineByteLengthLoadMetadataOpt = inlineByteLengthRangesOpt match {
      case Some(inlineByteLengthRanges) => Map("range" -> inlineByteLengthRanges)
      case None                         => Map[String, Metadata]()
    }

    // Load the inline length
    val inlineByteLengthIr = ct.StringCell.genLoadFromInlineByteLength(entryBlock)(
      stringIr,
      inlineByteLengthLoadMetadataOpt
    )

    // Check if we loaded a heap length
    val heapStringInlineByteLengthIr = IntegerConstant(
      ct.StringCell.inlineByteLengthIrType,
      ct.StringCellConstants.heapStringInlineByteLength
    )

    val isHeapIr = entryBlock.icmp("isHeap")(
      IComparisonCond.Equal,
      signed=None,
      heapStringInlineByteLengthIr,
      inlineByteLengthIr
    )

    // Branch on the heap-ness
    val isHeapStringBlock = irFunction.startChildBlock("isHeapString")
    val isInlineStringBlock = irFunction.startChildBlock("isInlineString")
    val stringCharLengthResultBlock = irFunction.startChildBlock("stringCharLengthResult")

    entryBlock.condBranch(isHeapIr, isHeapStringBlock, isInlineStringBlock)

    // Cast the string to a heap string
    val heapStringPtrType = PointerType(ct.HeapStringCell.irType)
    val heapStringIr = isHeapStringBlock.bitcastTo("heapString")(stringIr, heapStringPtrType)

    // Load the length
    val heapCharLengthIr = ct.HeapStringCell.genLoadFromHeapCharLength(isHeapStringBlock)(heapStringIr)

    isHeapStringBlock.uncondBranch(stringCharLengthResultBlock)

    // Cast the string to an inline string
    val inlineStringPtrType = PointerType(ct.InlineStringCell.irType)
    val inlineStringIr = isInlineStringBlock.bitcastTo("inlineString")(stringIr, inlineStringPtrType)

    // Load the length
    val inlineCharLengthLoadMetadata = Map("range" -> RangeMetadata(
      ct.InlineStringCell.inlineCharLengthIrType,
      // It takes at least one byte to encode a character
      (0, ct.StringCellConstants.maxInlineStringBytes + 1)
    ))

    val inlineCharLengthIr = ct.InlineStringCell.genLoadFromInlineCharLength(isInlineStringBlock)(
      inlineStringIr,
      inlineCharLengthLoadMetadata
    )

    // Zero extend it to match the heap type
    val extendedInlineCharLengthIr = isInlineStringBlock.zextTo("extendedInlineCharLength")(
      inlineCharLengthIr,
      ct.HeapStringCell.heapCharLengthIrType
    )

    isInlineStringBlock.uncondBranch(stringCharLengthResultBlock)

    // Phi the heap and inline lengths
    val resultIr = stringCharLengthResultBlock.phi("stringCharLength")(
      PhiSource(heapCharLengthIr, isHeapStringBlock),
      PhiSource(extendedInlineCharLengthIr, isInlineStringBlock)
    )

    val finalState = initialState.copy(currentBlock=stringCharLengthResultBlock)
    (finalState, resultIr)
  }
}
