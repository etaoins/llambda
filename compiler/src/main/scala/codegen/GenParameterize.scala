package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._
import llambda.llvmir.IrFunction._

object GenParameterize {
  private val dynamicenvPushDecl = RuntimeFunctions.dynamicenvPush
  private val dynamicenvSetValueDecl = RuntimeFunctions.dynamicenvSetValue 
  private val dynamicenvPopDecl = RuntimeFunctions.dynamicenvPop 

  private def declareSupportFunctions(state : GenerationState) {
    val module = state.currentBlock.function.module
    
    // Declare all support functions
    for(supportFunc <- List(dynamicenvPushDecl, dynamicenvSetValueDecl, dynamicenvPopDecl)) {
      module.unlessDeclared(supportFunc) {
        module.declareFunction(supportFunc)
      }
    }
  }

  def genPush(state : GenerationState)(step : ps.PushDynamicState) {
    declareSupportFunctions(state)

    // Get our world pointer IR value
    val worldPtrIr = state.liveTemps(step.worldPtr)

    val block = state.currentBlock
    
    // Push the new environment
    block.callDecl(None)(dynamicenvPushDecl, List(worldPtrIr))

    // Set each value
    for((parameterTemp, valueTemp) <- step.parameterValues) {
      val parameterIr = state.liveTemps(parameterTemp) 
      val valueIr = state.liveTemps(valueTemp) 

      block.callDecl(None)(dynamicenvSetValueDecl, List(worldPtrIr, parameterIr, valueIr))
    }
  }
  
  def genPop(state : GenerationState)(step : ps.PopDynamicState) {
    declareSupportFunctions(state)
          
    // Pop the environment
    val worldPtrIr = state.liveTemps(step.worldPtr)
    state.currentBlock.callDecl(None)(dynamicenvPopDecl, List(worldPtrIr))
  }
}
