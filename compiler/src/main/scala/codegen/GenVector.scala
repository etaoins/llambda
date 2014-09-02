package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenVector {
  case class InitVectorResult(
      finalState : GenerationState,
      vectorIr : IrValue,
      elementsIr : IrValue
  )

  def init(state : GenerationState)(lengthIr : IrValue) : InitVectorResult = {
    val block = state.currentBlock
    val module = block.function.module
    val allocation = state.currentAllocation

    // Allocate our vector elements
    val vectorElementAllocDecl = RuntimeFunctions.vectorElementsAlloc

    module.unlessDeclared(vectorElementAllocDecl) {
      module.declareFunction(vectorElementAllocDecl)
    }
    
    val elementsIr = block.callDecl(Some("vectorElements"))(vectorElementAllocDecl, List(lengthIr)).get
      
    // Grab our cell allocation
    val (newAllocation, vectorIr) = allocation.consumeCells(block)(1, ct.VectorCell)
    
    ct.VectorCell.genStoreToLength(block)(lengthIr, vectorIr)
    ct.VectorCell.genStoreToElements(block)(elementsIr, vectorIr)

    InitVectorResult(
      finalState=state.copy(currentAllocation=newAllocation),
      vectorIr=vectorIr,
      elementsIr=elementsIr
    )
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
