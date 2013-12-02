package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}

object GenTestBoxedRecordClass {
  def apply(block : IrBlockBuilder)(boxedRecord : IrValue, testClassId : Long) : IrValue = {
    // Load the actual class ID
    val actualClassIdIr = bt.BoxedRecord.genLoadFromRecordClassId(block)(boxedRecord)

    // Now compare
    val testClassIdIr = IntegerConstant(bt.BoxedRecord.recordClassIdIrType, testClassId)
    block.icmp("classMatches")(ComparisonCond.Equal, None, testClassIdIr, actualClassIdIr)
  }
}

