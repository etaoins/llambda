package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenLoadSymbolByte {
  def apply(block : IrBlockBuilder)(
      symbolIr : IrValue,
      offsetIr : IrValue,
      symbolByteLength : Long,
      possibleValuesOpt : Option[Set[Byte]]
  ) : IrValue = {
    val bytePtr = if (symbolByteLength <= ConstantGenerator.maximumInlineSymbolBytes) {
      val inlineSymbolIr = ct.InlineSymbolCell.genPointerBitcast(block)(symbolIr)

      // We can use GEP to directly index our byte
      val gepIndices = ct.InlineSymbolCell.inlineDataGepIndices.map(IntegerConstant(IntegerType(32), _)) :+ offsetIr
      block.getelementptr("bytePtr")(IntegerType(8), inlineSymbolIr, gepIndices)

    }
    else {
      // Indirect through the SharedByteArray pointer
      val heapSymbolIr = ct.HeapSymbolCell.genPointerBitcast(block)(symbolIr)
      val sharedByteArrayIr = ct.HeapSymbolCell.genLoadFromHeapByteArray(block)(heapSymbolIr)

      // Load our byte
      val gepIndices = List(0, 1).map(IntegerConstant(IntegerType(32), _)) :+ offsetIr
      block.getelementptr("bytePtr")(IntegerType(8), sharedByteArrayIr, gepIndices)
    }

    val rangeMetadataOpt = possibleValuesOpt flatMap { case possibleValues =>
      RangeMetadata.fromPossibleValues(
        integerType=IntegerType(8),
        possibleValues.map(_.toLong)
      )
    }

    val loadMetadata = rangeMetadataOpt match {
      case Some(rangeMetadata) => Map("range" -> rangeMetadata)
      case _ =>                   Map[String, Metadata]()
    }

    block.load("byteValue")(bytePtr, metadata=loadMetadata)
  }
}
