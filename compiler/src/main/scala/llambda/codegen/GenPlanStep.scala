package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{celltype => ct}

object GenPlanStep {
  def apply(state : GenerationState, plannedSymbols : Set[String], typeGenerator : TypeGenerator)(step : ps.Step) : GenerationState = step match {
    case ps.AllocateCells(tempAlloc, count) =>
      val (allocState, allocation) = GenCellAllocation(state)(count)
      allocState.withAllocation(tempAlloc -> allocation)

    case storeConstantStep : ps.StoreConstant =>
      val irResult = GenConstant(state)(storeConstantStep)

      state.withTempValue(storeConstantStep.result -> irResult)

    case unboxValueStep : ps.UnboxValue =>
      val irBoxed = state.liveTemps(unboxValueStep.boxed)
      val irResult = GenUnboxing(state)(unboxValueStep, irBoxed)

      state.withTempValue(unboxValueStep.result -> irResult)
    
    case boxValueStep : ps.BoxValue =>
      val irUnboxed = state.liveTemps(boxValueStep.unboxed)
      val irResult = GenBoxing(state)(boxValueStep, irUnboxed)

      state.withTempValue(boxValueStep.result -> irResult)

    case ps.StoreNamedEntryPoint(resultTemp, signature, nativeSymbol) =>
      val irValue = GenNamedEntryPoint(state.module)(signature, nativeSymbol, plannedSymbols)

      state.withTempValue((resultTemp -> irValue))

    case ps.CastCellToTypeUnchecked(resultTemp, subvalueTemp, targetType) =>
      val subvalueIr = state.liveTemps(subvalueTemp)

      val irValue = subvalueIr match {
        case constant : IrConstant =>
          // We can use an anonymous constant bitcast here
          BitcastToConstant(constant, PointerType(targetType.irType))

        case _ =>
          targetType.genPointerBitcast(state.currentBlock)(subvalueIr)
      }
      
      state.withTempValue((resultTemp -> irValue))

    case ps.CastCellToSubtypeChecked(resultTemp, supervalueTemp, targetType) =>
      val supervalueIr = state.liveTemps(supervalueTemp)

      val (successBlock, subvalueIr) = GenCastCellToSubtype(state)(supervalueIr, targetType)

      state.withTempValue(resultTemp -> subvalueIr).copy(currentBlock=successBlock)

    case ps.ConvertNativeInteger(resultTemp, fromValueTemp, toBits, signed) =>
      val fromValueIr = state.liveTemps(fromValueTemp)

      val convertedIr = GenIntegerConversion(state.currentBlock)(fromValueIr, toBits, signed)
      
      state.withTempValue(resultTemp -> convertedIr)
    
    case ps.ConvertNativeIntegerToFloat(resultTemp, fromValueTemp, fromSigned, fpType) =>
      val fromValueIr = state.liveTemps(fromValueTemp)
      val resultIr = GenIntegerToFloatConversion(state.currentBlock)(fromValueIr, fromSigned, fpType)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.ConvertNativeFloat(resultTemp, fromValueTemp, fpType) =>
      val fromValueIr = state.liveTemps(fromValueTemp)
      val block = state.currentBlock
      
      val resultIr = GenFloatConversion(state.currentBlock)(fromValueIr, fpType)

      state.withTempValue(resultTemp -> resultIr)

    case ps.BuildProperList(resultTemp, allocTemp, allocBase, valueTemps) =>
      val allocation = state.liveAllocations(allocTemp)
      val valueIrs = valueTemps.map(state.liveTemps.apply)

      val properListIr = GenProperList(state.currentBlock)(allocation, allocBase, valueIrs)

      state.withTempValue(resultTemp -> properListIr)

    case ps.TestCellType(resultTemp, cellTemp, cellType) =>
      val cellIr = state.liveTemps(cellTemp)

      // Load the type ID
      val block = state.currentBlock
      val datumIr = ct.DatumCell.genPointerBitcast(block)(cellIr)
      val typeIdIr = ct.DatumCell.genLoadFromTypeId(block)(datumIr)

      val resultIr = block.icmp(cellType.name + "Check")(ComparisonCond.Equal, None, typeIdIr, IntegerConstant(ct.DatumCell.typeIdIrType, cellType.typeId))

      state.withTempValue(resultTemp -> resultIr)

    case ps.CondBranch(resultTemp, testTemp, trueSteps, trueTemp, falseSteps, falseTemp) =>
      val testIr = state.liveTemps(testTemp)
      
      // Make two blocks
      val trueStartBlock = state.currentBlock.startChildBlock("condTrue")
      val falseStartBlock = state.currentBlock.startChildBlock("condFalse")

      // Branch!
      state.currentBlock.condBranch(testIr, trueStartBlock, falseStartBlock)

      // Continue generation down both branches after splitting our state
      val trueStartState = state.copy(currentBlock=trueStartBlock)
      val falseStartState = state.copy(currentBlock=falseStartBlock)

      val trueEndState = GenPlanSteps(trueStartState, plannedSymbols, typeGenerator)(trueSteps)
      val trueEndBlock = trueEndState.currentBlock

      val falseEndState = GenPlanSteps(falseStartState, plannedSymbols, typeGenerator)(falseSteps)
      val falseEndBlock = falseEndState.currentBlock
      
      // Get the IR values from either side
      val trueIrValue = trueEndState.liveTemps(trueTemp)
      val falseIrValue = falseEndState.liveTemps(falseTemp)

      // Make a final block
      val phiBlock = state.currentBlock.startChildBlock("condPhi")
      trueEndBlock.uncondBranch(phiBlock)
      falseEndBlock.uncondBranch(phiBlock)

      // Make the phi
      val phiValueIr = phiBlock.phi("condPhiResult")(PhiSource(trueIrValue, trueEndBlock), PhiSource(falseIrValue, falseEndBlock))

      state.copy(currentBlock=phiBlock).withTempValue(resultTemp -> phiValueIr) 

    case ps.Invoke(resultOpt, signature, funcPtrTemp, argumentTemps) =>
      val irSignature = ProcedureSignatureToIr(signature)
      val irFuncPtr = state.liveTemps(funcPtrTemp)
      val irArguments = argumentTemps.map(state.liveTemps.apply)

      val irRetOpt = state.currentBlock.call(Some("ret"))(irSignature, irFuncPtr, irArguments)

      resultOpt match {
        case Some(resultTemp) =>
          state.withTempValue(resultTemp -> irRetOpt.get)

        case None =>
          state
      }

    case ps.Return(None) =>
      state.currentBlock.retVoid()
      state
    
    case ps.Return(Some(returnValueTemp)) =>
      val irRetValue = state.liveTemps(returnValueTemp)

      state.currentBlock.ret(irRetValue)
      state

    case ps.StorePairCar(resultTemp, pairTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val carIr = ct.PairCell.genLoadFromCar(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> carIr)
    
    case ps.StorePairCdr(resultTemp, pairTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val cdrIr = ct.PairCell.genLoadFromCdr(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> cdrIr)

    case ps.StoreProcedureEntryPoint(resultTemp, procTemp) =>
      val procIr = state.liveTemps(procTemp)
      val entryPoint = ct.ProcedureCell.genLoadFromEntryPoint(state.currentBlock)(procIr)

      state.withTempValue(resultTemp -> entryPoint)

    case initStep : ps.RecordLikeInit  =>
      val initedRecordLike = GenRecordLikeInit(state, typeGenerator)(initStep)

      state
        .withTempValue(initStep.cellResult -> initedRecordLike.recordCell)
        .withTempValue(initStep.dataResult -> initedRecordLike.recordData)
    
    case ps.TestRecordLikeClass(resultTemp, recordCellTemp, recordLikeType) => 
      val generatedType = typeGenerator(recordLikeType)

      val recordCellIr = state.liveTemps(recordCellTemp)
      val irResult = GenTestRecordLikeClass(state.currentBlock)(recordCellIr, generatedType)

      state.withTempValue(resultTemp -> irResult)
    
    case ps.AssertRecordLikeClass(recordCellTemp, recordLikeType) => 
      val generatedType = typeGenerator(recordLikeType)
      
      // Start our branches
      val fatalBlock = state.currentBlock.startChildBlock("wrongRecordClass")
      val successBlock = state.currentBlock.startChildBlock("correctRecordClass")

      // Generate code for a fatal error
      val errorName = "recordClassIsNot" + generatedType.irType.name
      val errorText = "Record is not of class " + recordLikeType.sourceName

      GenFatalError(state.module, fatalBlock)(errorName, errorText)

      // Branch if we're not of the right class
      val recordCellIr = state.liveTemps(recordCellTemp)
      val irResult = GenTestRecordLikeClass(state.currentBlock)(recordCellIr, generatedType)

      state.currentBlock.condBranch(irResult, successBlock, fatalBlock)

      // Continue with the successful block
      state.copy(currentBlock=successBlock)
  
    case ps.RecordDataFieldSet(recordDataTemp, recordType, recordField, newValueTemp) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
      val newValueIr = state.liveTemps(newValueTemp)
      val generatedType = typeGenerator(recordType)
  
      GenRecordDataFieldSet(state.currentBlock)(recordDataIr, generatedType, recordField,newValueIr)

      state
    
    case ps.RecordDataFieldRef(resultTemp, recordDataTemp, recordLikeType, recordField) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
      val generatedType = typeGenerator(recordLikeType)
  
      val resultIr = GenRecordDataFieldRef(state.currentBlock)(recordDataIr, generatedType, recordField)

      state.withTempValue(resultTemp -> resultIr)

    case ps.StoreRecordLikeData(resultTemp, recordCellTemp, recordLikeType) =>
      val recordCellIr = state.liveTemps(recordCellTemp)
      val generatedType = typeGenerator(recordLikeType)

      val resultIr = GenStoreRecordLikeData(state.currentBlock)(recordCellIr, generatedType)

      state.withTempValue(resultTemp -> resultIr)
      
    case ps.SetProcedureEntryPoint(procedureCellTemp, entryPointTemp) =>
      val procedureCellIr = state.liveTemps(procedureCellTemp)
      val entryPointIr = state.liveTemps(entryPointTemp)

      // Store the entry point
      ct.ProcedureCell.genStoreToEntryPoint(state.currentBlock)(entryPointIr, procedureCellIr)

      state
 }
}
