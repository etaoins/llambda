package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._
import llambda.llvmir.IrFunction._

object GenParameter {
  private val dynamicenvPushDecl = RuntimeFunctions.dynamicenvPush
  private val dynamicenvSetValueDecl = RuntimeFunctions.dynamicenvSetValue 
  private val dynamicenvPopDecl = RuntimeFunctions.dynamicenvPop 
  private val makeParameterDecl = RuntimeFunctions.makeParameter
  private val valueForParameterDecl = RuntimeFunctions.valueForParameter

  private def declareSupportFunctions(state : GenerationState) {
    val module = state.currentBlock.function.module
    
    // Declare all support functions
    val supportFuncs = List(dynamicenvPushDecl, dynamicenvSetValueDecl, dynamicenvPopDecl, makeParameterDecl,
                            valueForParameterDecl)

    for(supportFunc <- supportFuncs) {
      module.unlessDeclared(supportFunc) {
        module.declareFunction(supportFunc)
      }
    }
  }

  def genPushDynamicState(state : GenerationState)(step : ps.PushDynamicState) : GenerationState = {
    declareSupportFunctions(state)

    val worldPtrIr = state.liveTemps(step.worldPtr)

    // States are wrapped in cells to suppoort GC - this means we need to insert a GC barrier here
    val (postPushState, _) = GenGcBarrier(state) {
      val block = state.currentBlock

      // Push the new environment
      block.callDecl(None)(dynamicenvPushDecl, List(worldPtrIr))

      (block, Unit)
    }
    
    // Set each value
    // This may invoke a converter procedure so we need a GC barrier
    step.parameterValues.foldLeft(postPushState) { case (state, paramValue) =>
      // Pull out our values
      val parameterIr = state.liveTemps(paramValue.parameterProc) 
      val valueIr = state.liveTemps(paramValue.newValue) 

      if (paramValue.mayHaveConverterProc) {
        // We need a GC barrier here for safety
        GenGcBarrier(state) {
          val entryBlock = state.currentBlock
          val successBlock = entryBlock.function.startChildBlock("setValueSuccess")

          entryBlock.invokeDecl(None)(
            decl=dynamicenvSetValueDecl,
            arguments=List(worldPtrIr, parameterIr, valueIr),
            normalBlock=successBlock,
            exceptionBlock=state.gcCleanUpBlockOpt.get
          )

          (successBlock, Unit)
        }._1
      }
      else {
        val block = state.currentBlock
        block.callDecl(None)(dynamicenvSetValueDecl, List(worldPtrIr, parameterIr, valueIr))
        state
      }
    }
  }
  
  def genPopDynamicState(state : GenerationState)(step : ps.PopDynamicState) : GenerationState = {
    declareSupportFunctions(state)
          
    // Pop the environment
    val worldPtrIr = state.liveTemps(step.worldPtr)
    state.currentBlock.callDecl(None)(dynamicenvPopDecl, List(worldPtrIr))

    state
  }
      
  def genCreateParameterProc(state : GenerationState)(
      worldPtrIr : IrValue,
      initialValueIr : IrValue,
      converterProcOpt : Option[IrValue]
  ) : (GenerationState, IrValue) = {
    val entryBlock = state.currentBlock

    declareSupportFunctions(state)

    // #!unit means "use identity function"
    val converterProcUncast = converterProcOpt.getOrElse(GlobalDefines.unitIrValue)

    // Cast to any*
    val converterProcCast = ct.AnyCell.genPointerBitcast(entryBlock)(converterProcUncast)

    // This will allocate
    GenGcBarrier(state) {
      val successBlock = state.currentBlock.function.startChildBlock("makeParameterSuccess") 

      val resultIr = entryBlock.invokeDecl(Some("parameterProc"))(
        decl=makeParameterDecl,
        arguments=List(worldPtrIr, initialValueIr, converterProcCast),
        normalBlock=successBlock,
        exceptionBlock=state.gcCleanUpBlockOpt.get
      )

      (successBlock, resultIr.get)
    }
  }

  def genLoadValueForParameterProc(state : GenerationState)(
      worldPtrIr : IrValue,
      parameterProcIr : IrValue
  ) : IrValue = {
    // This isn't a GC barrier thankfully
    val block = state.currentBlock

    block.callDecl(Some("parameterValue"))(valueForParameterDecl, List(worldPtrIr, parameterProcIr)).get
  }
}
