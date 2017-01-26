package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenParameter {
  private val dynamicenvPushDecl = RuntimeFunctions.dynamicenvPush
  private val dynamicenvSetValueDecl = RuntimeFunctions.dynamicenvSetValue
  private val dynamicenvPopDecl = RuntimeFunctions.dynamicenvPop
  private val makeParameterDecl = RuntimeFunctions.makeParameter
  private val valueForParameterDecl = RuntimeFunctions.valueForParameter

  private def declareSupportFunctions(state: GenerationState) {
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

  def genPushDynamicState(state: GenerationState)(step: ps.PushDynamicState): GenerationState = {
    declareSupportFunctions(state)

    val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
    val block = state.currentBlock

    // Push the new environment
    block.callDecl(None)(dynamicenvPushDecl, List(worldPtrIr))

    // Set each value
    for(paramValue <- step.parameterValues) {
      // Pull out our values
      val parameterIr = state.liveTemps(paramValue.parameterProc)
      val valueIr = state.liveTemps(paramValue.newValue)

      block.callDecl(None)(dynamicenvSetValueDecl, List(worldPtrIr, parameterIr, valueIr))
    }

    state
  }

  def genPopDynamicState(state: GenerationState)(step: ps.PopDynamicState): GenerationState = {
    declareSupportFunctions(state)

    // Pop the environment
    val worldPtrIr = state.liveTemps(ps.WorldPtrValue)
    state.currentBlock.callDecl(None)(dynamicenvPopDecl, List(worldPtrIr))

    state
  }

  def genCreateParameterProc(state: GenerationState)(
      worldPtrIr: IrValue,
      initialValueIr: IrValue
  ): (GenerationState, IrValue) = {
    val entryBlock = state.currentBlock

    declareSupportFunctions(state)

    // This will allocate
    GenGcBarrier(state) {
      val successBlock = state.currentBlock.function.startChildBlock("makeParameterSuccess")

      val resultIr = entryBlock.invokeDecl(Some("parameterProc"))(
        decl=makeParameterDecl,
        arguments=List(worldPtrIr, initialValueIr),
        normalBlock=successBlock,
        exceptionBlock=state.gcCleanUpBlockOpt.get
      )

      (successBlock, resultIr.get)
    }
  }

  def genLoadValueForParameterProc(state: GenerationState)(
      worldPtrIr: IrValue,
      parameterProcIr: IrValue
  ): IrValue = {
    // This isn't a GC barrier as we know this is a valid parameter procedure and we aren't passing arguments
    val block = state.currentBlock

    // Pass an empty rest argument as valueForParameterDecl needs to be compatible with the top procedure type
    val emptyListIr = GlobalDefines.emptyListIrValue
    val emptyListCastIr = BitcastToConstant(emptyListIr, PointerType(ct.ListElementCell.irType))

    val arguments = List(worldPtrIr, parameterProcIr, emptyListCastIr)
    block.callDecl(Some("parameterValue"))(valueForParameterDecl, arguments).get
  }
}
