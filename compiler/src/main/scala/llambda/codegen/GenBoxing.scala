package llambda.codegen

import llambda.InternalCompilerErrorException

import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{celltype => ct}

object GenBoxing {
  private val llibyStringFromUtf8Decl = IrFunctionDecl(
    result=IrFunction.Result(PointerType(ct.StringCell.irType)),
    name="_lliby_string_from_utf8",
    arguments=List(IrFunction.Argument(PointerType(IntegerType(8)))),
    attributes=Set(IrFunction.NoUnwind)
  )

  def apply(state : GenerationState, recordTypeGenerator : RecordTypeGenerator)(boxStep : ps.BoxValue, bativeValue : IrValue) : IrValue = boxStep match {
    case _ : ps.BoxBoolean =>
      state.currentBlock.select("boxedBool")(bativeValue, GlobalDefines.trueIrValue, GlobalDefines.falseIrValue)

    case ps.BoxExactInteger(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedIntCons = allocation.genTypedPointer(block)(allocIndex, ct.ExactIntegerCell) 
      ct.ExactIntegerCell.genStoreToValue(block)(bativeValue, boxedIntCons)

      boxedIntCons
    
    case ps.BoxInexactRational(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedRationalCons = allocation.genTypedPointer(block)(allocIndex, ct.InexactRationalCell) 
      ct.InexactRationalCell.genStoreToValue(block)(bativeValue, boxedRationalCons)

      boxedRationalCons

    case ps.BoxCharacter(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedCharCons = allocation.genTypedPointer(block)(allocIndex, ct.CharacterCell) 
      ct.CharacterCell.genStoreToUnicodeChar(block)(bativeValue, boxedCharCons)

      boxedCharCons

    case _ : ps.BoxUtf8String =>
      val block = state.currentBlock

      state.module.unlessDeclared(llibyStringFromUtf8Decl) {
        state.module.declareFunction(llibyStringFromUtf8Decl)
      }

      block.callDecl(Some("boxedString"))(llibyStringFromUtf8Decl, List(bativeValue)).get

    case ps.BoxProcedure(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedProcCons = allocation.genTypedPointer(block)(allocIndex, ct.ProcedureCell) 
      
      // XXX: Implement closures
      val constantZero = IntegerConstant(ct.ProcedureCell.recordClassIdIrType, 0)
      ct.ProcedureCell.genStoreToRecordClassId(block)(constantZero, boxedProcCons)

      val constantNull = NullPointerConstant(PointerType(IntegerType(8)))
      ct.ProcedureCell.genStoreToRecordData(block)(constantNull, boxedProcCons)

      // Store the entry point
      ct.ProcedureCell.genStoreToEntryPoint(block)(bativeValue, boxedProcCons)

      boxedProcCons

    case ps.BoxRecord(_, allocTemp, allocIndex, recordType, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedRecordCons = allocation.genTypedPointer(block)(allocIndex, ct.RecordCell) 
      
      // Get our record type information
      val generatedRecordType = recordTypeGenerator(recordType)

      // Set the class ID
      val classIdIr = IntegerConstant(ct.RecordCell.recordClassIdIrType, generatedRecordType.classId)
      ct.RecordCell.genStoreToRecordClassId(block)(classIdIr, boxedRecordCons)

      // Cast the data pointer to void*
      val voidDataPtr = block.bitcastTo("voidDataPtr")(bativeValue, PointerType(IntegerType(8)))
      ct.RecordCell.genStoreToRecordData(block)(voidDataPtr, boxedRecordCons)

      boxedRecordCons
  }
}


