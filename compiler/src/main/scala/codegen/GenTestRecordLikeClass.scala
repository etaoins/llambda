package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}

object GenTestRecordLikeClass {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, generatedType : GeneratedType, possibleTypesOpt : Option[Set[GeneratedType]] = None) : IrValue = {
    // Find the cell type of our record cell
    val cellType = generatedType.recordLikeType.cellType

    val loadMetadata = possibleTypesOpt match {
      case Some(possibleTypes) =>
        val rangeMetadata = RangeMetadata.fromPossibleValues(
          integerType=cellType.recordClassIdIrType,
          possibleTypes.map(_.classId)
        )

        Map("range" -> rangeMetadata)

      case _ =>
        // No types provided
        Map[String, RangeMetadata]()
    }

    // Load the actual class ID
    val actualClassIdIr = cellType.genLoadFromRecordClassId(block)(recordCellIr, loadMetadata)

    // Now compare
    val testClassIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
    block.icmp("classMatches")(ComparisonCond.Equal, None, testClassIdIr, actualClassIdIr)
  }
}

