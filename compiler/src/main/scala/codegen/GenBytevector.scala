package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}


object GenBytevector {
  private val sharedByteArrayDataTbaaNode = NumberedMetadata(5)

  private def storeElement(block: IrBlockBuilder)(byteArrayIr: IrValue, indexIr: IrValue, newValueIr: IrValue): Unit = {
    val elementPtrIr = block.getelementptr("elementPtr")(
      elementType=IntegerType(8),
      basePointer=byteArrayIr,
      indices=List(0, 2).map(IntegerConstant(IntegerType(32), _)) :+ indexIr,
      inbounds=true
    )

    block.store(newValueIr, elementPtrIr, metadata=Map("tbaa" -> sharedByteArrayDataTbaaNode))
  }

  def initStatic(state: GenerationState)(
    worldPtrIr: IrValue,
    elements: Vector[Byte]
  ): (GenerationState, IrValue) = {
    val block = state.currentBlock
    val module = block.function.module

    // Make a constant shared byte array
    val sharedByteArrayName = module.nameSource.allocate("staticElementsByteArray")
    val sharedByteArrayInitialiser = SharedByteArrayValue.createArrayConstant(elements)

    val constantDataDef = IrGlobalVariableDef(
      name=sharedByteArrayName,
      initializer=sharedByteArrayInitialiser,
      linkage=Linkage.Private,
      unnamedAddr=true,
      constant=true
    )

    module.defineGlobalVariable(constantDataDef)

    val sharedByteArrayIr = BitcastToConstant(constantDataDef.variable, PointerType(SharedByteArrayValue.irType))

    // Allocate the cell
    val allocation = state.currentAllocation
    val (newAllocation, bytevectorCellIr) = allocation.consumeCells(block)(1, ct.BytevectorCell)

    // Wire up the values
    val lengthIr = IntegerConstant(ct.VectorCell.lengthIrType, elements.length)

    ct.BytevectorCell.genStoreToLength(block)(lengthIr, bytevectorCellIr)
    ct.BytevectorCell.genStoreToByteArray(block)(sharedByteArrayIr, bytevectorCellIr)

    val newState = state.copy(currentAllocation=newAllocation)
    (newState, bytevectorCellIr)
  }

  def initDynamic(state: GenerationState)(worldPtrIr: IrValue, elementIrs: Vector[IrValue]): IrValue = {
    val block = state.currentBlock
    val module = block.function.module

    // Declare our support function
    val bytevectorAllocDecl = RuntimeFunctions.bytevectorAlloc

    module.unlessDeclared(bytevectorAllocDecl) {
      module.declareFunction(bytevectorAllocDecl)
    }

    // Allocate the bytevector uninitialised
    val lengthIr = IntegerConstant(ct.VectorCell.lengthIrType, elementIrs.length)
    val allocArgs = List(worldPtrIr, lengthIr)
    val bytevectorCellIr = block.callDecl(Some("newBytevector"))(bytevectorAllocDecl, allocArgs).get

    // Set our element values
    val byteArrayIr = ct.BytevectorCell.genLoadFromByteArray(block)(bytevectorCellIr)
    for((elementIr, index) <- elementIrs.zipWithIndex) {
      val indexIr = IntegerConstant(ct.VectorCell.lengthIrType, index)
      storeElement(state.currentBlock)(byteArrayIr, indexIr, elementIr)
    }

    bytevectorCellIr
  }

  def initFilled(state: GenerationState)(worldPtrIr: IrValue, lengthIr: IrValue, fillIr: IrValue): IrValue = {
    val block = state.currentBlock
    val module = block.function.module

    // Declare our support function
    val bytevectorAllocFilledDecl = RuntimeFunctions.bytevectorAllocFilled

    module.unlessDeclared(bytevectorAllocFilledDecl) {
      module.declareFunction(bytevectorAllocFilledDecl)
    }

    // Allocate the bytevector filled
    val allocArgs = List(worldPtrIr, lengthIr, fillIr)
    block.callDecl(Some("newBytevector"))(bytevectorAllocFilledDecl, allocArgs).get
  }

  def loadElement(block: IrBlockBuilder)(byteArrayIr: IrValue, indexIr: IrValue): IrValue =
    SharedByteArrayValue.genLoadFromDataByte(block)(byteArrayIr, indexIr)
}
