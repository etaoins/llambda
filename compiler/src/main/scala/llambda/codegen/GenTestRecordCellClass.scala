package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{celltype => ct}

object GenTestRecordCellClass {
  def apply(block : IrBlockBuilder)(recordCellIr : IrValue, testClassId : Long) : IrValue = {
    // Load the actual class ID
    val actualClassIdIr = ct.RecordCell.genLoadFromRecordClassId(block)(recordCellIr)

    // Now compare
    val testClassIdIr = IntegerConstant(ct.RecordCell.recordClassIdIrType, testClassId)
    block.icmp("classMatches")(ComparisonCond.Equal, None, testClassIdIr, actualClassIdIr)
  }
}

