package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.{celltype => ct}

object GenCastCellToSubtype {
  def apply(state : GenerationState)(
      worldPtr : IrValue,
      supertypeValue : IrValue,
      targetType : ct.CellType,
      errorMessage : RuntimeErrorMessage,
      possibleTypes : Set[ct.ConcreteCellType]
  ) : (IrBlockBuilder, IrValue) = {
    val irFunction = state.currentBlock.function 
    val successBlock = irFunction.startChildBlock(targetType.llvmName + "SubcastSuccess") 
    val failBlock = irFunction.startChildBlock(targetType.llvmName + "SubcastFail") 

    // Build range metadata from our possible types
    val rangeMetadata = RangeMetadata.fromPossibleValues(
      integerType=ct.DatumCell.typeIdIrType,
      possibleTypes.map(_.typeId)
    )
    val loadMetadata = Map("range" -> rangeMetadata)

    // Do the actual check
    targetType.genTypeCheck(state.currentBlock)(supertypeValue, successBlock, failBlock, loadMetadata)
  
    // Generate the fail branch
    {
      val failState = state.copy(currentBlock=failBlock)

      val irritant = ct.DatumCell.genPointerBitcast(failBlock)(supertypeValue)
      GenErrorSignal(failState)(worldPtr, errorMessage, Some(irritant))
    }

    // Continue building on the success block
    val subtypeValue = targetType.genPointerBitcast(successBlock)(supertypeValue)

    (successBlock, subtypeValue)
  }
}
