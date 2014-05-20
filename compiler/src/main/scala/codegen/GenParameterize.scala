package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._
import llambda.llvmir.IrFunction._

object GenParameterize {
  private val llibyDynamicenvPush = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_push",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )

  private val llibyDynamicenvSetValue = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_set_value",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(PointerType(ct.ProcedureCell.irType)),
      Argument(PointerType(ct.DatumCell.irType))
    ),
    attributes=Set(NoUnwind)
  )
  
  private val llibyDynamicenvPop = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_pop",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )

  private def declareSupportFunctions(state : GenerationState) {
      val module = state.currentBlock.function.module
      
      // Declare all support functions
      for(supportFunc <- List(llibyDynamicenvPush, llibyDynamicenvSetValue, llibyDynamicenvPop)) {
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
    block.callDecl(None)(llibyDynamicenvPush, List(worldPtrIr))

    // Set each value
    for((parameterTemp, valueTemp) <- step.parameterValues) {
      val parameterIr = state.liveTemps(parameterTemp) 
      val valueIr = state.liveTemps(valueTemp) 

      block.callDecl(None)(llibyDynamicenvSetValue, List(worldPtrIr, parameterIr, valueIr))
    }
  }
  
  def genPop(state : GenerationState)(step : ps.PopDynamicState) {
    declareSupportFunctions(state)
          
    // Pop the environment
    val worldPtrIr = state.liveTemps(step.worldPtr)
    state.currentBlock.callDecl(None)(llibyDynamicenvPop, List(worldPtrIr))
  }
}
