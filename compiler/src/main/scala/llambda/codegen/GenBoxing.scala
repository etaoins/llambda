package llambda.codegen

import llambda.InternalCompilerErrorException

import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}

object GenBoxing {
  private val llibyStringFromUtf8Decl = IrFunctionDecl(
    result=IrFunction.Result(PointerType(bt.BoxedString.irType)),
    name="_lliby_string_from_utf8",
    arguments=List(IrFunction.Argument(PointerType(IntegerType(8)))),
    attributes=Set(IrFunction.NoUnwind)
  )

  def apply(state : GenerationState, recordTypeGenerator : RecordTypeGenerator)(boxStep : ps.BoxValue, unboxedValue : IrValue) : IrValue = boxStep match {
    case _ : ps.BoxBoolean =>
      state.currentBlock.select("boxedBool")(unboxedValue, GlobalDefines.trueIrValue, GlobalDefines.falseIrValue)

    case ps.BoxExactInteger(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedIntCons = allocation.genTypedPointer(block)(allocIndex, bt.BoxedExactInteger) 
      bt.BoxedExactInteger.genStoreToValue(block)(unboxedValue, boxedIntCons)

      boxedIntCons
    
    case ps.BoxInexactRational(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedRationalCons = allocation.genTypedPointer(block)(allocIndex, bt.BoxedInexactRational) 
      bt.BoxedInexactRational.genStoreToValue(block)(unboxedValue, boxedRationalCons)

      boxedRationalCons

    case ps.BoxCharacter(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedCharCons = allocation.genTypedPointer(block)(allocIndex, bt.BoxedCharacter) 
      bt.BoxedCharacter.genStoreToUnicodeChar(block)(unboxedValue, boxedCharCons)

      boxedCharCons

    case _ : ps.BoxUtf8String =>
      val block = state.currentBlock

      state.module.unlessDeclared(llibyStringFromUtf8Decl) {
        state.module.declareFunction(llibyStringFromUtf8Decl)
      }

      block.callDecl(Some("boxedString"))(llibyStringFromUtf8Decl, List(unboxedValue)).get

    case ps.BoxProcedure(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedProcCons = allocation.genTypedPointer(block)(allocIndex, bt.BoxedProcedure) 
      
      // XXX: Implement closures
      val constantZero = IntegerConstant(bt.BoxedProcedure.recordClassIdIrType, 0)
      bt.BoxedProcedure.genStoreToRecordClassId(block)(constantZero, boxedProcCons)

      val constantNull = NullPointerConstant(PointerType(IntegerType(8)))
      bt.BoxedProcedure.genStoreToRecordData(block)(constantNull, boxedProcCons)

      // Store the entry point
      bt.BoxedProcedure.genStoreToEntryPoint(block)(unboxedValue, boxedProcCons)

      boxedProcCons

    case ps.BoxRecord(_, allocTemp, allocIndex, recordType, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedRecordCons = allocation.genTypedPointer(block)(allocIndex, bt.BoxedRecord) 
      
      // Get our record type information
      val generatedRecordType = recordTypeGenerator(recordType)

      // Set the class ID
      val classIdIr = IntegerConstant(bt.BoxedRecord.recordClassIdIrType, generatedRecordType.classId)
      bt.BoxedRecord.genStoreToRecordClassId(block)(classIdIr, boxedRecordCons)

      // Cast the data pointer to void*
      val voidDataPtr = block.bitcastTo("voidDataPtr")(unboxedValue, PointerType(IntegerType(8)))
      bt.BoxedRecord.genStoreToRecordData(block)(voidDataPtr, boxedRecordCons)

      boxedRecordCons
  }
}


