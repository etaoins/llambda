package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenVector {
  def init(state : GenerationState)(worldPtrIr : IrValue, lengthIr : IrValue) : (GenerationState, IrValue) = {
    val func = state.currentBlock.function
    val module = func.module

    val vectorAllocDecl = RuntimeFunctions.vectorAlloc

    module.unlessDeclared(vectorAllocDecl) {
      module.declareFunction(vectorAllocDecl)
    }

    GenGcBarrier(state) {
      val entryBlock = state.currentBlock
      val successBlock = func.startChildBlock("vectorAllocSuccess")

      val newVectorIr = entryBlock.invokeDecl(Some("newVector"))(
        decl=vectorAllocDecl,
        arguments=List(worldPtrIr, lengthIr),
        normalBlock=successBlock,
        exceptionBlock=state.gcCleanUpBlockOpt.get
      ).get

      (successBlock, newVectorIr)
    }
  }

  def loadElement(block : IrBlockBuilder)(elementsIr : IrValue, indexIr : IrValue) : IrValue = {
    val elementPtrIr = block.getelementptr("elementPtr")(
      elementType=PointerType(ct.AnyCell.irType),
      basePointer=elementsIr,
      indices=Vector(indexIr),
      inbounds=true
    )

    block.load("element")(elementPtrIr)
  }
  
  def storeElement(block : IrBlockBuilder)(elementsIr : IrValue, indexIr : IrValue, newValueIr : IrValue) : Unit = {
    val elementPtrIr = block.getelementptr("elementPtr")(
      elementType=PointerType(ct.AnyCell.irType),
      basePointer=elementsIr,
      indices=Vector(indexIr),
      inbounds=true
    )

    block.store(newValueIr, elementPtrIr)
  }
}
