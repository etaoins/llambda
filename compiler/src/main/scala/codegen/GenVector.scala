package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

object GenVector {
  import Implicits._

  private val vectorDataTbaaNode = NumberedMetadata(6)

  private def createUninitialised(state: GenerationState)(
      worldPtrIr: IrValue,
      lengthIr: IrValue
  ): IrValue = {
    val func = state.currentBlock.function
    val module = func.module

    val vectorAllocDecl = RuntimeFunctions.vectorAlloc

    module.unlessDeclared(vectorAllocDecl) {
      module.declareFunction(vectorAllocDecl)
    }

    val entryBlock = state.currentBlock

    entryBlock.callDecl(Some("newVector"))(vectorAllocDecl, List(worldPtrIr, lengthIr)).get
  }

  def init(state: GenerationState)(
      worldPtrIr: IrValue,
      elementTemps: Vector[ps.TempValue]
  ): IrValue = {
    val lengthIr = IntegerConstant(ct.VectorCell.lengthIrType, elementTemps.length)

    val vectorCellIr = createUninitialised(state)(worldPtrIr, lengthIr)
    val dataIr = ct.VectorCell.genLoadFromElements(state.currentBlock)(vectorCellIr)

    for((elementTemp, index) <- elementTemps.zipWithIndex) {
      val indexIr = IntegerConstant(ct.VectorCell.lengthIrType, index)
      val elementIr = state.liveTemps(elementTemp)

      storeElement(state.currentBlock)(dataIr, indexIr, elementIr)
    }

    vectorCellIr
  }

  def initFilled(state: GenerationState)(
      worldPtrIr: IrValue,
      lengthIr: IrValue,
      fillTemp: ps.TempValue
  ): (GenerationState, IrValue) = {
    val vectorCellIr = createUninitialised(state)(worldPtrIr, lengthIr)
    val dataIr = ct.VectorCell.genLoadFromElements(state.currentBlock)(vectorCellIr)

    val previousBlock = state.currentBlock
    val func = previousBlock.function

    // It's impossible for this to wrap
    val wrapBehaviour = Set[WrapBehaviour](WrapBehaviour.NoSignedWrap, WrapBehaviour.NoUnsignedWrap)

    // Create our blocks
    val rangeCheckBlock = func.startChildBlock("initVectorRangeCheck")
    val bodyBlock = func.startChildBlock("initVectorBody")
    val exitBlock = func.startChildBlock("initVectorExit")

    val indexType = ct.VectorCell.lengthIrType
    val incedIndex = LocalVariable(func.nameSource.allocate("incedIndex"), indexType)

    previousBlock.uncondBranch(rangeCheckBlock)

    // Find our loop index
    val indexIr = rangeCheckBlock.phi("index")(
      PhiSource(IntegerConstant(indexType, 0), previousBlock),
      PhiSource(incedIndex, bodyBlock)
    )

    // See if we're out of range
    val indexExhaustedIr = rangeCheckBlock.icmp("indexExhausted")(
      compareCond=IComparisonCond.GreaterThanEqual,
      signed=Some(false),
      val1=indexIr,
      val2=lengthIr
    )

    rangeCheckBlock.condBranch(indexExhaustedIr, exitBlock, bodyBlock)

    // Set the element
    val fillIr = state.liveTemps(fillTemp)
    storeElement(bodyBlock)(dataIr, indexIr, fillIr)

    // Increment our index
    bodyBlock.add(incedIndex)(wrapBehaviour, indexIr, IntegerConstant(indexType, 1))
    bodyBlock.uncondBranch(rangeCheckBlock)

    (state.copy(currentBlock=exitBlock), vectorCellIr)
  }

  def loadElement(block: IrBlockBuilder)(dataIr: IrValue, indexIr: IrValue): IrValue = {
    val elementPtrIr = block.getelementptr("elementPtr")(
      elementType=PointerType(ct.AnyCell.irType),
      basePointer=dataIr,
      indices=Vector(indexIr),
      inbounds=true
    )

    block.load("element")(elementPtrIr, metadata=Map("tbaa" -> vectorDataTbaaNode))
  }

  def storeElement(block: IrBlockBuilder)(dataIr: IrValue, indexIr: IrValue, newValueIr: IrValue): Unit = {
    val elementPtrIr = block.getelementptr("elementPtr")(
      elementType=PointerType(ct.AnyCell.irType),
      basePointer=dataIr,
      indices=Vector(indexIr),
      inbounds=true
    )

    block.store(newValueIr, elementPtrIr, metadata=Map("tbaa" -> vectorDataTbaaNode))
  }
}
