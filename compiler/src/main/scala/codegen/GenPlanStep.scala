package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.ProcedureAttribute
import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenPlanStep {
  // Our runtime doesn't have defined wrap behaviour so we don't either
  // We only do math on signed values so nsw is sufficient here
  private val integerWrapBehaviour = Set(
    WrapBehaviour.NoSignedWrap
  ) : Set[WrapBehaviour]

  private val fastMathFlags = Set[FastMathFlag]()

  private def stepCompareCondToIntegerIr(stepCond : ps.CompareCond) : IComparisonCond = stepCond match {
    case ps.CompareCond.Equal => IComparisonCond.Equal
    case ps.CompareCond.NotEqual => IComparisonCond.NotEqual
    case ps.CompareCond.GreaterThan => IComparisonCond.GreaterThan
    case ps.CompareCond.GreaterThanEqual => IComparisonCond.GreaterThanEqual
    case ps.CompareCond.LessThan => IComparisonCond.LessThan
    case ps.CompareCond.LessThanEqual => IComparisonCond.LessThanEqual
  }

  private def stepCompareCondToFloatIr(stepCond : ps.CompareCond) : FComparisonCond = stepCond match {
    case ps.CompareCond.Equal => FComparisonCond.OrderedEqual
    case ps.CompareCond.NotEqual => FComparisonCond.OrderedNotEqual
    case ps.CompareCond.GreaterThan => FComparisonCond.OrderedGreaterThan
    case ps.CompareCond.GreaterThanEqual => FComparisonCond.OrderedGreaterThanEqual
    case ps.CompareCond.LessThan => FComparisonCond.OrderedLessThan
    case ps.CompareCond.LessThanEqual => FComparisonCond.OrderedLessThanEqual
  }
  
  def apply(state : GenerationState, genGlobals : GenGlobals)(step : ps.Step) : GenResult = {
    genGlobals.debugInfoGeneratorOpt match {
      case Some(debugInfoGenerator) =>
        // Add our line numbering debug info if possible
        val debugMetadata = debugInfoGenerator.metadataForContextLocated(step)

        state.currentBlock.function.withMetadata(debugMetadata) {
          genStep(state, genGlobals)(step)
        }

      case None =>
        genStep(state, genGlobals)(step)
    }
  }

  private def genStep(state : GenerationState, genGlobals : GenGlobals)(step : ps.Step) : GenResult = step match {
    case ps.AllocateCells(count) =>
      if (!state.currentAllocation.isEmpty) {
        // This is not only wasteful but dangerous as the previous allocation won't be fully initialized
        throw new InternalCompilerErrorException("Attempted cell allocation without fully consuming previous allocation")
      }

      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)

      val (allocState, allocation) = GenCellAllocation.genAllocation(state)(worldPtrIr, count)
      allocState.copy(currentAllocation=allocation)

    case createConstantStep : ps.CreateConstant =>
      val irResult = genGlobals.constantGenerator(state, genGlobals)(createConstantStep)

      state.withTempValue(createConstantStep.result -> irResult)

    case unboxValueStep : ps.UnboxValue =>
      val irBoxed = state.liveTemps(unboxValueStep.boxed)
      val irResult = GenUnboxing(state)(unboxValueStep, irBoxed)

      state.withTempValue(unboxValueStep.result -> irResult)
    
    case boxValueStep : ps.BoxValue =>
      val irUnboxed = state.liveTemps(boxValueStep.unboxed)
      val (boxState, irResult) = GenBoxing(state)(boxValueStep, irUnboxed)

      boxState.withTempValue(boxValueStep.result -> irResult)

    case ps.CreateNamedEntryPoint(resultTemp, signature, nativeSymbol) =>
      val irModule = state.currentBlock.function.module
      val irValue = GenNamedEntryPoint(irModule)(signature, nativeSymbol, genGlobals.plannedSymbols)

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
      
      state.copy(
        liveTemps=state.liveTemps.withAliasedTempValue(subvalueTemp, (resultTemp -> irValue))
      )

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
    
    case ps.CalcProperListLength(resultTemp, listHeadTemp) =>
      val listHeadIr = state.liveTemps(listHeadTemp)
      val (listState, lengthIr) = GenCalcProperListLength(state)(listHeadIr)

      listState.withTempValue(resultTemp -> lengthIr)

    case ps.TestCellType(resultTemp, cellTemp, cellType, possibleTypes) =>
      val cellIr = state.liveTemps(cellTemp)

      // Build range metadata from our possible types
      val rangeMetadata = RangeMetadata.fromPossibleValues(
        integerType=ct.AnyCell.typeIdIrType,
        possibleTypes.map(_.typeId)
      )
      val loadMetadata = Map("range" -> rangeMetadata)

      // Load the type ID
      val block = state.currentBlock
      val datumIr = ct.AnyCell.genPointerBitcast(block)(cellIr)
      val typeIdIr = ct.AnyCell.genLoadFromTypeId(block)(datumIr, metadata=loadMetadata)

      val resultIr = block.icmp(cellType.llvmName + "Check")(IComparisonCond.Equal, None, typeIdIr, IntegerConstant(ct.AnyCell.typeIdIrType, cellType.typeId))

      state.withTempValue(resultTemp -> resultIr)

    case condBranch : ps.CondBranch =>
      GenCondBranch(state, genGlobals)(condBranch)

    case invokeStep @ ps.Invoke(resultOpt, signature, funcPtrTemp, arguments, inputToDispose, _) =>
      val result = ProcedureSignatureToIr(signature)
      val irSignature = result.irSignature
      val metadata = result.callMetadata

      val irFuncPtr = state.liveTemps(funcPtrTemp)
      val irArguments = arguments.map(state.liveTemps)

      val preBarrierState = state.withDisposedValues(inputToDispose)

      val (finalState, irRetOpt) = if (invokeStep.canAllocate) {
        if (signature.attributes.contains(ProcedureAttribute.NoReturn)) {
          // This can't return - unroot all of our values and terminate the function
          return state.terminateFunction(() => {
            preBarrierState.currentBlock.call(None)(irSignature, irFuncPtr, irArguments)
            preBarrierState.currentBlock.unreachable
          })
        }
        else {
          // We need a GC barrier
          GenGcBarrier(preBarrierState) {
            val invokeBlock = preBarrierState.currentBlock
            val successBlock = invokeBlock.function.startChildBlock("invokeSuccess") 

            val irValue = invokeBlock.invoke(Some("invokeRet"))(
              signature=irSignature,
              functionPtr=irFuncPtr,
              arguments=irArguments,
              normalBlock=successBlock,
              exceptionBlock=preBarrierState.gcCleanUpBlockOpt.get,
              metadata=metadata
            )

            (successBlock, irValue)
          }
        }
      }
      else {
        // This call can't allocate or throw exceptions - skip the barrier and invoke
        val block = preBarrierState.currentBlock
        val irValue = block.call(Some("ret"))(irSignature, irFuncPtr, irArguments, metadata=metadata)

        (preBarrierState, irValue)
      }

      resultOpt match {
        case Some(resultTemp) =>
          finalState.withTempValue(resultTemp -> irRetOpt.get)

        case None =>
          finalState
      }

    case ps.TailCall(signature, funcPtrTemp, arguments) =>
      val result = ProcedureSignatureToIr(signature)
      val irSignature = result.irSignature
      val metadata = result.callMetadata

      val irFuncPtr = state.liveTemps(funcPtrTemp)
      val irArguments = arguments.map(state.liveTemps)

      // Always use call - we don't care about exceptions
      state.terminateFunction({ () => 
        if (irSignature.result.irType == VoidType) {
          state.currentBlock.call(None)(irSignature, irFuncPtr, irArguments, tailCall=true)
          state.currentBlock.retVoid()
        }
        else {
          val block = state.currentBlock
          val irRetValueOpt = block.call(Some("ret"))(
            irSignature,
            irFuncPtr,
            irArguments,
            tailCall=true,
            metadata=metadata
          )

          block.ret(irRetValueOpt.get)
        }
      })

    case ps.Return(None) =>
      state.terminateFunction(() => {
        state.currentBlock.retVoid()
      })
    
    case ps.Return(Some(returnValueTemp)) =>
      val irRetValue = state.liveTemps(returnValueTemp)

      state.terminateFunction(() => {
        state.currentBlock.ret(irRetValue)
      })

    case ps.LoadPairCar(resultTemp, pairTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val carIr = ct.PairCell.genLoadFromCar(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> carIr)
    
    case ps.LoadPairCdr(resultTemp, pairTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val cdrIr = ct.PairCell.genLoadFromCdr(state.currentBlock)(pairIr)

      state.withTempValue(resultTemp -> cdrIr)

    case ps.LoadStringCharLength(resultTemp, stringTemp) =>
      val stringIr = state.liveTemps(stringTemp)
      val lengthIr = ct.StringCell.genLoadFromCharLength(state.currentBlock)(stringIr)

      state.withTempValue(resultTemp -> lengthIr)

    case ps.LoadSymbolByteLength(resultTemp, stringTemp, possibleLengthsOpt) =>
      val symbolIr = state.liveTemps(stringTemp)

      val loadMetadata = possibleLengthsOpt match {
        case Some(possibleLengths) =>
          Map("range" -> RangeMetadata.fromPossibleValues(
            integerType=ct.SymbolCell.byteLengthIrType,
            possibleLengths.map(_.toLong)
          ))

        case _ =>
          Map[String, Metadata]()
      }

      val lengthIr = ct.SymbolCell.genLoadFromByteLength(state.currentBlock)(symbolIr, loadMetadata)

      state.withTempValue(resultTemp -> lengthIr)

    case ps.LoadSymbolByte(resultTemp, symbolTemp, offsetTemp, symbolByteLength, possibleValuesOpt) =>
      val block = state.currentBlock
      val symbolIr = state.liveTemps(symbolTemp)
      val offsetIr = state.liveTemps(offsetTemp)

      val byteIr = GenLoadSymbolByte(state.currentBlock)(symbolIr, offsetIr, symbolByteLength, possibleValuesOpt)

      state.withTempValue(resultTemp -> byteIr)

    case ps.LoadBytevectorLength(resultTemp, bytevectorTemp) =>
      val bytevectorIr = state.liveTemps(bytevectorTemp)
      val lengthIr = ct.BytevectorCell.genLoadFromLength(state.currentBlock)(bytevectorIr)

      state.withTempValue(resultTemp -> lengthIr)

    case ps.InitVector(resultTemp, elements) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)

      val (finalState, vectorIr) = GenVector.init(state)(worldPtrIr, elements)

      finalState.withTempValue(resultTemp -> vectorIr)

    case ps.InitFilledVector(resultTemp, length, fill) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val lengthIr = state.liveTemps(length)

      val (finalState, vectorIr) = GenVector.initFilled(state)(worldPtrIr, lengthIr, fill)

      finalState.withTempValue(resultTemp -> vectorIr)

    case ps.LoadVectorElementsData(resultTemp, vectorTemp) =>
      val vectorIr = state.liveTemps(vectorTemp)
      val elementsIr = ct.VectorCell.genLoadFromElements(state.currentBlock)(vectorIr)

      state.withTempValue(resultTemp -> elementsIr)

    case ps.LoadVectorLength(resultTemp, vectorTemp) =>
      val vectorIr = state.liveTemps(vectorTemp)
      val lengthIr = ct.VectorCell.genLoadFromLength(state.currentBlock)(vectorIr)

      state.withTempValue(resultTemp -> lengthIr)
    
    case ps.LoadVectorElement(resultTemp, _, elementsTemp, indexTemp) =>
      val elementsIr = state.liveTemps(elementsTemp)
      val indexIr = state.liveTemps(indexTemp)

      val elementIr = GenVector.loadElement(state.currentBlock)(elementsIr, indexIr)
      state.withTempValue(resultTemp -> elementIr)
    
    case ps.StoreVectorElement(_, elementsTemp, indexTemp, newValueTemp) =>
      val elementsIr = state.liveTemps(elementsTemp)
      val indexIr = state.liveTemps(indexTemp)
      val newValueIr = state.liveTemps(newValueTemp)

      GenVector.storeElement(state.currentBlock)(elementsIr, indexIr, newValueIr)
      state

    case ps.LoadProcedureEntryPoint(resultTemp, procTemp, signature) =>
      val procIr = state.liveTemps(procTemp)
      val signatureIrType =  ProcedureSignatureToIr(signature).irSignature.irType
      val block = state.currentBlock

      // Load the entry point
      val entryPointIr = ct.ProcedureCell.genLoadFromEntryPoint(block)(procIr)
      // Cast it to the expected entry point type 
      val castEntryPointIr = block.bitcastTo("castEntryPoint")(entryPointIr, PointerType(signatureIrType))

      state.withTempValue(resultTemp -> castEntryPointIr)

    case initStep : ps.InitRecord =>
      val (initedState, resultIr) = GenInitRecordLike(state, genGlobals.generatedTypes)(initStep)

      initedState
        .withTempValue(initStep.result -> resultIr)

    case initStep : ps.InitProcedure =>
      val (initedState, resultIr) = GenInitRecordLike(state, genGlobals.generatedTypes)(initStep)

      // Store the entry point
      val entryPointIr = state.liveTemps(initStep.entryPoint)
      val block = state.currentBlock

      val castEntryPointIr = block.bitcastTo("castEntryPoint")(entryPointIr, ct.ProcedureCell.entryPointIrType)
      ct.ProcedureCell.genStoreToEntryPoint(state.currentBlock)(castEntryPointIr, resultIr)

      initedState
        .withTempValue(initStep.result -> resultIr)

    case ps.TestRecordLikeClass(resultTemp, recordCellTemp, recordLikeType, possibleTypesOpt) => 
      val generatedType = genGlobals.generatedTypes(recordLikeType)
      val generatedPossibleTypes = possibleTypesOpt map { possibleTypes =>
        possibleTypes.map(genGlobals.generatedTypes)
      }

      val recordCellIr = state.liveTemps(recordCellTemp)
      val irResult = GenTestRecordLikeClass(state.currentBlock)(recordCellIr, generatedType, generatedPossibleTypes)

      state.withTempValue(resultTemp -> irResult)
    
    case ps.SetRecordLikeDefined(recordCellTemp, recordLikeType) => 
      val generatedType = genGlobals.generatedTypes(recordLikeType)
      
      // Mark ourselves as defined
      val recordCellIr = state.liveTemps(recordCellTemp)
      val cellType = generatedType.recordLikeType.cellType
      val irResult = cellType.genStoreToIsUndefined(state.currentBlock)(IntegerConstant(cellType.isUndefinedIrType, 0), recordCellIr)

      state

    case ps.AssertRecordLikeDefined(recordCellTemp, recordLikeType, errorMessage) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val generatedType = genGlobals.generatedTypes(recordLikeType)

      // Start our branches
      val irFunction = state.currentBlock.function
      val fatalBlock = irFunction.startChildBlock("recordIsUndefined")
      val successBlock = irFunction.startChildBlock("recordIsDefined")

      GenErrorSignal(state.copy(currentBlock=fatalBlock))(worldPtrIr, errorMessage, locatedOpt=Some(step))

      // Branch if we're defined
      val recordCellIr = state.liveTemps(recordCellTemp)
      val cellType = generatedType.recordLikeType.cellType

      val loadMetadata = Map("range" -> RangeMetadata(ct.AnyCell.gcStateIrType, (0, 2)))
      val isUndefinedBool = cellType.genLoadFromIsUndefined(state.currentBlock)(recordCellIr, loadMetadata)

      val isUndefinedPred = state.currentBlock.truncTo("undefinedPred")(isUndefinedBool, IntegerType(1))

      state.currentBlock.condBranch(isUndefinedPred, fatalBlock, successBlock)

      // Continue with the successful block
      state.copy(currentBlock=successBlock)

    case ps.SetRecordDataField(recordDataTemp, recordType, recordField, newValueTemp) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
      val newValueIr = state.liveTemps(newValueTemp)
      val generatedType = genGlobals.generatedTypes(recordType)
  
      GenSetRecordDataField(state.currentBlock)(recordDataIr, generatedType, recordField, newValueIr)

      state
    
    case ps.LoadRecordDataField(resultTemp, recordDataTemp, recordLikeType, recordField) =>
      val recordDataIr = state.liveTemps(recordDataTemp)
      val generatedType = genGlobals.generatedTypes(recordLikeType)
  
      val resultIr = GenLoadRecordDataField(state.currentBlock)(recordDataIr, generatedType, recordField)

      state.withTempValue(resultTemp -> resultIr)

    case ps.LoadRecordLikeData(resultTemp, recordCellTemp, recordLikeType) =>
      val recordCellIr = state.liveTemps(recordCellTemp)
      val generatedType = genGlobals.generatedTypes(recordLikeType)

      val resultIr = GenLoadRecordLikeData(state.currentBlock)(recordCellIr, generatedType)

      state.withTempValue(resultTemp -> resultIr)
      
    case ps.DisposeValues(disposedTemps) =>
      state.copy(
        liveTemps=state.liveTemps -- disposedTemps
      )

    case pushDynamic : ps.PushDynamicState =>
      GenParameter.genPushDynamicState(state)(pushDynamic)
    
    case popDynamic : ps.PopDynamicState =>
      GenParameter.genPopDynamicState(state)(popDynamic)

    case ps.CreateParameterProc(resultTemp, initialValueTemp, converterProcTempOpt, inputToDispose) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val initialValueIr = state.liveTemps(initialValueTemp)
      val converterProcOptIr = converterProcTempOpt.map(state.liveTemps)

      val disposedState = state.withDisposedValues(inputToDispose)

      val (postProcState, resultIr) = GenParameter.genCreateParameterProc(disposedState)(
        worldPtrIr,
        initialValueIr,
        converterProcOptIr
      ) 

      postProcState.withTempValue(resultTemp -> resultIr)

    case ps.LoadValueForParameterProc(resultTemp, parameterProcTemp) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val parameterProcIr = state.liveTemps(parameterProcTemp)

      val resultIr = GenParameter.genLoadValueForParameterProc(state)(worldPtrIr, parameterProcIr) 
      state.withTempValue(resultTemp -> resultIr)

    case checkedStep @ ps.CheckedIntegerAdd(resultTemp, val1Temp, val2Temp, overflowMessage) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val (newState, resultIr) = GenCheckedIntegerInstr(state)(worldPtrIr, checkedStep, "add", val1Ir, val2Ir)

      newState.withTempValue(resultTemp -> resultIr)

    case checkedStep @ ps.CheckedIntegerSub(resultTemp, val1Temp, val2Temp, overflowMessage) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val (newState, resultIr) = GenCheckedIntegerInstr(state)(worldPtrIr, checkedStep, "sub", val1Ir, val2Ir)

      newState.withTempValue(resultTemp -> resultIr)

    case checkedStep @ ps.CheckedIntegerMul(resultTemp, val1Temp, val2Temp, overflowMessage) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val (newState, resultIr) = GenCheckedIntegerInstr(state)(worldPtrIr, checkedStep, "mul", val1Ir, val2Ir)

      newState.withTempValue(resultTemp -> resultIr)

    case ps.IntegerDiv(resultTemp, signed, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = if (signed) {
        state.currentBlock.sdiv("sdivResult")(false, val1Ir, val2Ir)
      }
      else {
        state.currentBlock.udiv("udivResult")(false, val1Ir, val2Ir)
      }

      state.withTempValue(resultTemp -> resultIr)

    case ps.IntegerRem(resultTemp, signed, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = if (signed) {
        state.currentBlock.srem("sremResult")(val1Ir, val2Ir)
      }
      else {
        state.currentBlock.urem("uremResult")(val1Ir, val2Ir)
      }

      state.withTempValue(resultTemp -> resultIr)
    case ps.FloatAdd(resultTemp, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = state.currentBlock.fadd("faddResult")(fastMathFlags, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.FloatSub(resultTemp, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = state.currentBlock.fsub("fsubResult")(fastMathFlags, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.FloatMul(resultTemp, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = state.currentBlock.fmul("fmulResult")(fastMathFlags, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.FloatDiv(resultTemp, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val resultIr = state.currentBlock.fdiv("fdivResult")(fastMathFlags, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.IntegerCompare(resultTemp, compareCond, signed, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val condIr = stepCompareCondToIntegerIr(compareCond)
      val resultIr = state.currentBlock.icmp("compResult")(condIr, signed, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)
    
    case ps.FloatCompare(resultTemp, compareCond, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val condIr = stepCompareCondToFloatIr(compareCond)
      val resultIr = state.currentBlock.fcmp("compResult")(condIr, val1Ir, val2Ir)

      state.withTempValue(resultTemp -> resultIr)

    case ps.FloatIsNaN(resultTemp, valTemp) =>
      val valIr = state.liveTemps(valTemp)
      
      // This is tricky - only NaN is not equal to itself. This is actually now isnan is implemented on OS X and LLVM
      // optimises it appropriately
      val resultIr = state.currentBlock.fcmp("isNaN")(FComparisonCond.UnorderedNotEqual, valIr, valIr)
      
      state.withTempValue(resultTemp -> resultIr)

    case ps.FloatBitwiseCompare(resultTemp, val1Temp, val2Temp) =>
      val val1Ir = state.liveTemps(val1Temp)
      val val2Ir = state.liveTemps(val2Temp)

      val block = state.currentBlock
      val val1IntCastIr = block.bitcastTo("val1IntCast")(val1Ir, IntegerType(64))
      val val2IntCastIr = block.bitcastTo("val2IntCast")(val2Ir, IntegerType(64))

      val resultIr = block.icmp("compResult")(IComparisonCond.Equal, None, val1IntCastIr, val2IntCastIr)
      state.withTempValue(resultTemp -> resultIr)

    case ps.InitPair(resultTemp, listLengthOpt) =>
      val block = state.currentBlock
      val allocation = state.currentAllocation
      
      val (newAllocation, resultIr) = allocation.consumeCells(block)(1, ct.PairCell)

      for(listLength <- listLengthOpt) {
        val listLengthIr = IntegerConstant(ct.PairCell.listLengthIrType, listLength)
        ct.PairCell.genStoreToListLength(state.currentBlock)(listLengthIr, resultIr)
      }
      
      state.copy(
        currentAllocation=newAllocation
      ).withTempValue(resultTemp -> resultIr)

    case ps.AssertPairMutable(pairTemp, errorMessage) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val pairIr = state.liveTemps(pairTemp)

      // Start our branches
      val irFunction = state.currentBlock.function
      val fatalBlock = irFunction.startChildBlock("pairIsImmutable")
      val successBlock = irFunction.startChildBlock("pairIsMutable")

      GenErrorSignal(state.copy(currentBlock=fatalBlock))(worldPtrIr, errorMessage, locatedOpt=Some(step))

      // The GC state can only be 0 (GC allocated) or 1 (global constant)
      // There are other GC states temporarily used during GC but they should never be reachable by program code
      val rangeMetadata = RangeMetadata(ct.AnyCell.gcStateIrType, (0, 2))

      val globalConstantGcState = IntegerConstant(ct.AnyCell.gcStateIrType, 1)
      val gcStateIr = ct.PairCell.genLoadFromGcState(state.currentBlock)(pairIr, metadata=Map("range" -> rangeMetadata))
      val irResult = state.currentBlock.icmp("isMutable")(IComparisonCond.NotEqual, None, globalConstantGcState, gcStateIr) 

      state.currentBlock.condBranch(irResult, successBlock, fatalBlock)

      // Continue with the successful block
      state.copy(currentBlock=successBlock)

    case ps.SetPairCar(pairTemp, newValueTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val newValueIr = state.liveTemps(newValueTemp)
      
      ct.PairCell.genStoreToCar(state.currentBlock)(newValueIr, pairIr)
      state

    case ps.SetPairCdr(pairTemp, newValueTemp) =>
      val pairIr = state.liveTemps(pairTemp)
      val newValueIr = state.liveTemps(newValueTemp)
      
      ct.PairCell.genStoreToCdr(state.currentBlock)(newValueIr, pairIr)
      state

    case ps.AssertPredicate(predicateTemp, errorMessage, evidenceOpt) =>
      val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
      val predIr = state.liveTemps(predicateTemp)
      val evidenceIrOpt = evidenceOpt.map(state.liveTemps)
      
      // Start our branches
      val irFunction = state.currentBlock.function
      val fatalBlock = irFunction.startChildBlock("predFalse")
      val successBlock = irFunction.startChildBlock("predTrue")

      GenErrorSignal(state.copy(currentBlock=fatalBlock))(worldPtrIr, errorMessage, evidenceIrOpt, locatedOpt=Some(step))

      state.currentBlock.condBranch(predIr, successBlock, fatalBlock)

      // Continue with the successful block
      state.copy(currentBlock=successBlock)
  }
}
