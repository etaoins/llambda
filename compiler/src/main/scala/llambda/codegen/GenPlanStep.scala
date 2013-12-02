package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.nfi
import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}

object GenPlanStep {
  def apply(state : GenerationState, plannedSymbols : Set[String], recordTypeGenerator : RecordTypeGenerator)(step : ps.Step) : GenerationState = step match {
    case ps.AllocateCons(tempAlloc, count) =>
      val (allocState, allocation) = GenConsAllocation(state)(count)
      allocState.withAllocation(tempAlloc -> allocation)

    case ps.MutableVarInit(resultTemp, tempAlloc, allocIndex) =>
      val allocation = state.liveAllocations(tempAlloc) 

      // Grab the variable from the cons allocation
      val mutableCons = allocation.genTypedPointer(state.currentBlock)(allocIndex, bt.BoxedMutableVar) 

      // Add it to our state
      state.withTempValue(resultTemp -> mutableCons)
    
    case ps.MutableVarSet(mutableTemp, newValueTemp) =>
      // Find our MutableVar's IrValue
      val mutableIr = state.liveTemps(mutableTemp)
      val newValueIr = state.liveTemps(newValueTemp)

      // Store to the mutable variable
      bt.BoxedMutableVar.genStoreToCurrentValue(state.currentBlock)(newValueIr, mutableIr)

      state

    case storeConstantStep : ps.StoreConstant =>
      val irResult = GenConstant(state)(storeConstantStep)

      state.withTempValue(storeConstantStep.result -> irResult)

    case unboxValueStep : ps.UnboxValue =>
      val irBoxed = state.liveTemps(unboxValueStep.boxed)
      val irResult = GenUnboxing(state)(unboxValueStep, irBoxed)

      state.withTempValue(unboxValueStep.result -> irResult)
    
    case boxValueStep : ps.BoxValue =>
      val irUnboxed = state.liveTemps(boxValueStep.unboxed)
      val irResult = GenBoxing(state, recordTypeGenerator)(boxValueStep, irUnboxed)

      state.withTempValue(boxValueStep.result -> irResult)

    case ps.MutableVarRef(resultTemp, mutableTemp) =>
      // Mutable variable. This is unfortunate
      val mutableIr = state.liveTemps(mutableTemp)

      val block = state.currentBlock
      val currentDatum = bt.BoxedMutableVar.genLoadFromCurrentValue(block)(mutableIr)

      state.withTempValue(resultTemp -> currentDatum)
    
    case ps.StoreNamedEntryPoint(resultTemp, signature, nativeSymbol) =>
      val irValue = GenNamedEntryPoint(state.module)(signature, nativeSymbol, plannedSymbols)

      state.withTempValue((resultTemp -> irValue))

    case ps.CastBoxedToTypeUnchecked(resultTemp, subvalueTemp, targetType) =>
      val subvalueIr = state.liveTemps(subvalueTemp)

      val irValue = subvalueIr match {
        case constant : IrConstant =>
          // We can use an anonymous constant bitcast here
          BitcastToConstant(constant, PointerType(targetType.irType))

        case _ =>
          targetType.genPointerBitcast(state.currentBlock)(subvalueIr)
      }
      
      state.withTempValue((resultTemp -> irValue))

    case ps.CastBoxedToSubtypeChecked(resultTemp, supervalueTemp, targetType) =>
      val supervalueIr = state.liveTemps(supervalueTemp)

      val (successBlock, subvalueIr) = GenCastBoxedToSubtype(state)(supervalueIr, targetType)

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

    case ps.TestBoxedType(resultTemp, boxedTemp, boxedType) =>
      val boxedIr = state.liveTemps(boxedTemp)

      // Load the type ID
      val block = state.currentBlock
      val datumIr = bt.BoxedDatum.genPointerBitcast(block)(boxedIr)
      val typeIdIr = bt.BoxedDatum.genLoadFromTypeId(block)(datumIr)

      val resultIr = block.icmp(boxedType.name + "Check")(ComparisonCond.Equal, None, typeIdIr, IntegerConstant(bt.BoxedDatum.typeIdIrType, boxedType.typeId))

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

      val trueEndState = GenPlanSteps(trueStartState, plannedSymbols, recordTypeGenerator)(trueSteps)
      val trueEndBlock = trueEndState.currentBlock

      val falseEndState = GenPlanSteps(falseStartState, plannedSymbols, recordTypeGenerator)(falseSteps)
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
      val irSignature = NativeSignatureToIr(signature)
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
      val carIr = bt.BoxedPair.genLoadFromCar(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> carIr)
    
    case ps.StorePairCdr(resultTemp, pairTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val cdrIr = bt.BoxedPair.genLoadFromCdr(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> cdrIr)

    case ps.StoreProcedureClosure(resultTemp, procTemp) =>
      val procIr = state.liveTemps(procTemp)
      val closureIr = bt.BoxedProcedure.genLoadFromRecordData(state.currentBlock)(procIr)

      state.withTempValue(resultTemp -> closureIr)
    
    case ps.StoreProcedureEntryPoint(resultTemp, procTemp) =>
      val procIr = state.liveTemps(procTemp)
      val entryPoint = bt.BoxedProcedure.genLoadFromEntryPoint(state.currentBlock)(procIr)

      state.withTempValue(resultTemp -> entryPoint)

    case ps.RecordDataAllocate(resultTemp, recordType) => 
      val generatedRecordType = recordTypeGenerator(recordType)

      val irResult = GenRecordDataAllocate(state.module, state.currentBlock)(generatedRecordType.irType)
      state.withTempValue(resultTemp -> irResult)
    
    case ps.TestBoxedRecordClass(resultTemp, boxedRecordTemp, recordType) => 
      val generatedRecordType = recordTypeGenerator(recordType)

      val boxedRecordIr = state.liveTemps(boxedRecordTemp)
      val irResult = GenTestBoxedRecordClass(state.currentBlock)(boxedRecordIr, generatedRecordType.classId)

      state.withTempValue(resultTemp -> irResult)
    
    case ps.AssertBoxedRecordClass(boxedRecordTemp, recordType) => 
      val generatedRecordType = recordTypeGenerator(recordType)
      
      // Start our branches
      val fatalBlock = state.currentBlock.startChildBlock("wrongRecordClass")
      val successBlock = state.currentBlock.startChildBlock("correctRecordClass")

      // Generate code for a fatal error
      val errorName = "recordClassIsNot" + generatedRecordType.irType.name
      val errorText = "Record is not of class " + recordType.sourceName

      GenFatalError(state.module, fatalBlock)(errorName, errorText)

      // Branch if we're not of the right class
      val boxedRecordIr = state.liveTemps(boxedRecordTemp)
      val irResult = GenTestBoxedRecordClass(state.currentBlock)(boxedRecordIr, generatedRecordType.classId)

      state.currentBlock.condBranch(irResult, successBlock, fatalBlock)

      // Continue with the successful block
      state.copy(currentBlock=successBlock)
  
    case ps.RecordFieldSet(recordDataTemp, recordType, recordField, newValueTemp) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
      val newValueIr = state.liveTemps(newValueTemp)
  
      GenRecordFieldSet(state.currentBlock)(recordDataIr, recordType, recordField, newValueIr)

      state
    
    case ps.RecordFieldRef(resultTemp, recordDataTemp, recordType, recordField) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
  
      val resultIr = GenRecordFieldRef(state.currentBlock)(recordDataIr, recordType, recordField)

      state.withTempValue(resultTemp -> resultIr)

    case ps.StoreBoxedRecordData(resultTemp, boxedRecordTemp, recordType) =>
      val generatedRecordType = recordTypeGenerator(recordType)
      val boxedRecordIr = state.liveTemps(boxedRecordTemp)

      val resultIr = GenStoreBoxedRecordData(state.currentBlock)(boxedRecordIr, generatedRecordType.irType)

      state.withTempValue(resultTemp -> resultIr)
 }
}
