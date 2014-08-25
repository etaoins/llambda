package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
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

  def genPush(state : GenerationState)(step : ps.PushDynamicState) : GenerationState = {
    declareSupportFunctions(state)

    val block = state.currentBlock
    val worldPtrIr = state.liveTemps(step.worldPtr)

    // States are wrapped in cells to suppoort GC - this means we need to insert a GC barrier here
    val (finalState, _)  = GenGcBarrier(state) {
      // Push the new environment
      block.callDecl(None)(dynamicenvPushDecl, List(worldPtrIr))

      (block, Unit)
    }
      
    // Set each value
    for((parameterTemp, valueTemp) <- step.parameterValues) {
      val parameterIr = finalState.liveTemps(parameterTemp) 
      val valueIr = finalState.liveTemps(valueTemp) 

      block.callDecl(None)(dynamicenvSetValueDecl, List(worldPtrIr, parameterIr, valueIr))
    }

    finalState
  }
  
  def genPop(state : GenerationState)(step : ps.PopDynamicState) : GenerationState = {
    declareSupportFunctions(state)
          
    // Pop the environment
    val worldPtrIr = state.liveTemps(step.worldPtr)
    state.currentBlock.callDecl(None)(dynamicenvPopDecl, List(worldPtrIr))

    state
  }
}
