package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenCastCellToSubtype {
  def apply(state : GenerationState)(supertypeValue : IrValue, targetType : ct.CellType) : (IrBlockBuilder, IrValue) = {
    val successBlock = state.currentBlock.startChildBlock(targetType.llvmName + "SubcastSuccess") 
    val failBlock = state.currentBlock.startChildBlock(targetType.llvmName + "SubcastFail") 

    // Do the actual check
    targetType.genTypeCheck(state.currentBlock)(supertypeValue, successBlock, failBlock)
  
    // Generate the fail branch
    val errorName = s"subcastTo${targetType.llvmName.capitalize}Failed"
    val errorMessage = s"Runtime cast to subtype '${targetType.llvmName}' failed" 
    GenFatalError(state.module, failBlock)(errorName, errorMessage)

    // Continue building on the success block
    val subtypeValue = targetType.genPointerBitcast(successBlock)(supertypeValue)

    (successBlock, subtypeValue)
  }
}
