package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}


object GenLoadSymbolByteLength {
  def apply(initialState: GenerationState)(
      symbolIr: IrValue,
      possibleLengthsOpt: Option[Set[Int]]
  ): (GenerationState, IrValue) = {
    // We expand both the inline and heap length to this size
    val targetType = ct.HeapSymbolCell.heapByteLengthIrType

    val possibleInlineLengthsOpt = possibleLengthsOpt map { possibleLengths =>
      possibleLengths map { possibleLength =>
        if (possibleLength > ct.SymbolCellConstants.maxInlineSymbolBytes) {
          // This is a heap length. We will record the inline heap length as heapSymbolInlineByteLength
          ct.SymbolCellConstants.heapSymbolInlineByteLength
        }
        else {
          possibleLength
        }
      }
    }

    val inlineRangeMetadataOpt = possibleInlineLengthsOpt flatMap { possibleInlineLengths =>
      RangeMetadata.fromPossibleValues(
        integerType=ct.SymbolCell.inlineByteLengthIrType,
        possibleInlineLengths.map(_.toLong)
      )
    }

    val inlineLoadMetadata = inlineRangeMetadataOpt match {
      case Some(inlineRangeMetadata) => Map("range" -> inlineRangeMetadata)
      case _ =>                         Map[String, Metadata]()
    }

    val possibleHeapLengthsOpt = possibleLengthsOpt map { possibleLengths =>
      possibleLengths filter (_ > ct.SymbolCellConstants.maxInlineSymbolBytes)
    }

    val entryBlock = initialState.currentBlock
    val irFunction = entryBlock.function

    // Load the inline length
    val inlineByteLengthIr = ct.SymbolCell.genLoadFromInlineByteLength(entryBlock)(symbolIr, inlineLoadMetadata)

    // Zero extend it to our target value
    val extendedInlineByteLengthIr = entryBlock.zextTo("extendedInlineByteLength")(
      inlineByteLengthIr,
      targetType
    )

    if (possibleHeapLengthsOpt == Some(Set())) {
      (initialState, extendedInlineByteLengthIr)
    }
    else {
      val heapRangeMetadataOpt = possibleHeapLengthsOpt flatMap { possibleHeapLengths =>
        RangeMetadata.fromPossibleValues(
          integerType=ct.HeapSymbolCell.heapByteLengthIrType,
          possibleHeapLengths.map(_.toLong)
        )
      }

      val heapLoadMetadata = heapRangeMetadataOpt match {
        case Some(heapRangeMetadata) => Map("range" -> heapRangeMetadata)
        case _ =>                       Map[String, Metadata]()
      }

      // Check if we loaded a heap length
      val heapSymbolInlineByteLengthIr = IntegerConstant(
        targetType,
        ct.SymbolCellConstants.heapSymbolInlineByteLength
      )

      val isHeapIr = entryBlock.icmp("isHeap")(
        IComparisonCond.Equal,
        signed=None,
        heapSymbolInlineByteLengthIr,
        extendedInlineByteLengthIr
      )

      // Branch on the heap-ness
      val isHeapSymbolBlock = irFunction.startChildBlock("isHeapSymbol")
      val symbolLengthResultBlock = irFunction.startChildBlock("symbolByteLengthResult")

      entryBlock.condBranch(isHeapIr, isHeapSymbolBlock, symbolLengthResultBlock)

      // Cast the symbol to a heap symbol
      val heapSymbolPtrType = PointerType(ct.HeapSymbolCell.irType)
      val heapSymbolIr = isHeapSymbolBlock.bitcastTo("heapSymbol")(symbolIr, heapSymbolPtrType)

      // Load the symbol length
      val heapByteLengthIr = ct.HeapSymbolCell.genLoadFromHeapByteLength(isHeapSymbolBlock)(
        heapSymbolIr,
        heapLoadMetadata
      )

      isHeapSymbolBlock.uncondBranch(symbolLengthResultBlock)

      // Phi the heap and inline lengths
      val resultIr = symbolLengthResultBlock.phi("symbolByteLength")(
        PhiSource(heapByteLengthIr, isHeapSymbolBlock),
        PhiSource(extendedInlineByteLengthIr, entryBlock)
      )

      val finalState = initialState.copy(currentBlock=symbolLengthResultBlock)
      (finalState, resultIr)
    }
  }
}
