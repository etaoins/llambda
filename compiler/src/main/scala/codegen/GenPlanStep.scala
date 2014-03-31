package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenPlanStep {
  def apply(state : GenerationState, plannedSymbols : Set[String], typeGenerator : TypeGenerator)(step : ps.Step) : GenResult = step match {
    case ps.AllocateCells(worldPtrTemp, count) =>
      if (!state.currentAllocation.isEmpty) {
        // This is not only wasteful but dangerous as the previous allocation
        // won't be fully initialized
        throw new InternalCompilerErrorException("Attempted cell allocation without fully consuming previous allocation")
      }

      val worldPtrIr = state.liveTemps(worldPtrTemp)

      val (allocState, allocation) = GenCellAllocation.genAllocation(state)(worldPtrIr, count)
      allocState.copy(currentAllocation=allocation)

    case storeConstantStep : ps.StoreConstant =>
      val irResult = GenConstant(state, typeGenerator)(storeConstantStep)

      state.withTempValue(storeConstantStep.result -> irResult)

    case unboxValueStep : ps.UnboxValue =>
      val irBoxed = state.liveTemps(unboxValueStep.boxed)
      val irResult = GenUnboxing(state)(unboxValueStep, irBoxed)

      state.withTempValue(unboxValueStep.result -> irResult)
    
    case boxValueStep : ps.BoxValue =>
      val irUnboxed = state.liveTemps(boxValueStep.unboxed)
      val (boxState, irResult) = GenBoxing(state)(boxValueStep, irUnboxed)

      boxState.withTempValue(boxValueStep.result -> irResult)

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

    case ps.CastCellToSubtypeChecked(resultTemp, worldPtrTemp, supervalueTemp, targetType, errorMessage) =>
      val worldPtrIr = state.liveTemps(worldPtrTemp)
      val supervalueIr = state.liveTemps(supervalueTemp)

      val (successBlock, subvalueIr) = GenCastCellToSubtype(state)(worldPtrIr, supervalueIr, targetType, errorMessage)

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
      val resultIr = GenFloatConversion(state.currentBlock)(fromValueIr, fpType)

      state.withTempValue(resultTemp -> resultIr)

    case ps.BuildProperList(resultTemp, valueTemps) =>
      val valueIrs = valueTemps.map(state.liveTemps.apply)

      val (listState, properListIr) = GenProperList(state)(valueIrs)

      listState.withTempValue(resultTemp -> properListIr)

    case ps.TestCellType(resultTemp, cellTemp, cellType) =>
      val cellIr = state.liveTemps(cellTemp)

      // Load the type ID
      val block = state.currentBlock
      val datumIr = ct.DatumCell.genPointerBitcast(block)(cellIr)
      val typeIdIr = ct.DatumCell.genLoadFromTypeId(block)(datumIr)

      val resultIr = block.icmp(cellType.llvmName + "Check")(ComparisonCond.Equal, None, typeIdIr, IntegerConstant(ct.DatumCell.typeIdIrType, cellType.typeId))

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

      val trueResult = GenPlanSteps(trueStartState, plannedSymbols, typeGenerator)(trueSteps)
      val falseResult = GenPlanSteps(falseStartState, plannedSymbols, typeGenerator)(falseSteps)

      (trueResult, falseResult) match {
        case (BlockTerminated, BlockTerminated) =>
          // Both branches terminated
          BlockTerminated

        case (trueEndState : GenerationState, BlockTerminated) =>
          // Only true terminated
          trueEndState.withTempValue(resultTemp -> trueEndState.liveTemps(trueTemp))
        
        case (BlockTerminated, falseEndState : GenerationState) =>
          // Only false terminated
          falseEndState.withTempValue(resultTemp -> falseEndState.liveTemps(falseTemp))

        case (trueEndState : GenerationState, falseEndState : GenerationState) =>
          // Neither branch terminated - we need to phi

          // Get the IR values from either side
          val trueEndBlock = trueEndState.currentBlock
          val trueIrValue = trueEndState.liveTemps(trueTemp)

          val falseEndBlock = falseEndState.currentBlock
          val falseIrValue = falseEndState.liveTemps(falseTemp)

          // Make a final block
          val phiBlock = state.currentBlock.startChildBlock("condPhi")
          trueEndBlock.uncondBranch(phiBlock)
          falseEndBlock.uncondBranch(phiBlock)

          // Make the phi
          val phiValueIr = phiBlock.phi("condPhiResult")(PhiSource(trueIrValue, trueEndBlock), PhiSource(falseIrValue, falseEndBlock))

          state.copy(
            currentBlock=phiBlock,
            // A value is only considered rooted if it was rooted in both branches:
            gcRootedTemps=(trueEndState.gcRootedTemps & falseEndState.gcRootedTemps)
          ).withTempValue(resultTemp -> phiValueIr) 
      }
      
    case invokeStep @ ps.Invoke(resultOpt, signature, funcPtrTemp, arguments) =>
      val irSignature = ProcedureSignatureToIr(signature)
      val irFuncPtr = state.liveTemps(funcPtrTemp)
      val irArguments = arguments.map { argument =>
        state.liveTemps(argument.tempValue)
      }

      // Dispose of arguments before our barrier
      // This prevents us from GC rooting them needlessly
      val disposedArgTemps = arguments.filter(_.dispose).map(_.tempValue)

      val preBarrierState = state.copy(
        liveTemps=state.liveTemps -- disposedArgTemps
      )

      val callBlock = () => {  
        val irValue = preBarrierState.currentBlock.call(Some("ret"))(irSignature, irFuncPtr, irArguments)
        (preBarrierState.currentBlock, irValue)
      }

      val (finalState, irRetOpt) = if (invokeStep.canAllocate) {
        // We need a GC barrier
        GenGcBarrier(preBarrierState) {
          val invokeBlock = preBarrierState.currentBlock
          val successBlock = invokeBlock.startChildBlock("invokeSuccess") 

          val irValue = invokeBlock.invoke(Some("invokeRet"))(
            signature=irSignature,
            functionPtr=irFuncPtr,
            arguments=irArguments,
            normalBlock=successBlock,
            exceptionBlock=preBarrierState.gcCleanUpBlockOpt.get
          )

          (successBlock, irValue)
        }
      }
      else {
        // This call can't allocate or throw exceptions - skip the barrier and invoke 
        val irValue = preBarrierState.currentBlock.call(Some("ret"))(irSignature, irFuncPtr, irArguments)
        (preBarrierState, irValue)
      }

      resultOpt match {
        case Some(resultTemp) =>
          finalState.withTempValue(resultTemp -> irRetOpt.get)

        case None =>
          finalState
      }

    case ps.Return(None) =>
      state.currentBlock.retVoid()

      // We returned - no more state
      BlockTerminated
    
    case ps.Return(Some(returnValueTemp)) =>
      val irRetValue = state.liveTemps(returnValueTemp)

      state.currentBlock.ret(irRetValue)

      // We returned - no more state
      BlockTerminated

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
      val (initedState, initedRecordLike) = GenRecordLikeInit(state, typeGenerator)(initStep)

      initedState
        .withTempValue(initStep.cellResult -> initedRecordLike.recordCell)
        .withTempValue(initStep.dataResult -> initedRecordLike.recordData)
    
    case ps.TestRecordLikeClass(resultTemp, recordCellTemp, recordLikeType) => 
      val generatedType = typeGenerator(recordLikeType)

      val recordCellIr = state.liveTemps(recordCellTemp)
      val irResult = GenTestRecordLikeClass(state.currentBlock)(recordCellIr, generatedType)

      state.withTempValue(resultTemp -> irResult)
    
    case ps.AssertRecordLikeClass(worldPtrTemp, recordCellTemp, recordLikeType, errorMessage) => 
      val worldPtrIr = state.liveTemps(worldPtrTemp)
      val generatedType = typeGenerator(recordLikeType)
      
      // Start our branches
      val fatalBlock = state.currentBlock.startChildBlock("wrongRecordClass")
      val successBlock = state.currentBlock.startChildBlock("correctRecordClass")

      GenErrorSignal(state.copy(currentBlock=fatalBlock))(worldPtrIr, errorMessage)

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

    case ps.DisposeValue(disposedTemp) =>
      state.copy(liveTemps=state.liveTemps - disposedTemp)

    case parameterize : ps.Parameterize =>
      GenParameterize(state, plannedSymbols, typeGenerator)(parameterize) 

 }
}
