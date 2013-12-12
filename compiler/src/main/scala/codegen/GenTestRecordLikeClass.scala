package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenTestRecordLikeClass {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, generatedType : GeneratedType) : IrValue = {
    // Load the actual class ID
    val cellType = generatedType.recordLikeType.cellType
    val actualClassIdIr = cellType.genLoadFromRecordClassId(block)(recordCellIr)

    // Now compare
    val testClassIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
    block.icmp("classMatches")(ComparisonCond.Equal, None, testClassIdIr, actualClassIdIr)
  }
}

