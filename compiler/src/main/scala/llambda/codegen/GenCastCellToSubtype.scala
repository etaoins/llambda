package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{celltype => ct}

object GenCastCellToSubtype {
  def apply(state : GenerationState)(supertypeValue : IrValue, targetType : ct.CellType) : (IrBlockBuilder, IrValue) = {
    val successBlock = state.currentBlock.startChildBlock(targetType.name + "SubcastSuccess") 
    val failBlock = state.currentBlock.startChildBlock(targetType.name + "SubcastFail") 

    // Do the actual check
    targetType.genTypeCheck(state.currentBlock)(supertypeValue, successBlock, failBlock)
  
    // Generate the fail branch
    val errorName = s"subcastTo${targetType.name.capitalize}Failed"
    val errorMessage = s"Runtime cast to subtype '${targetType.name}' failed" 
    GenFatalError(state.module, failBlock)(errorName, errorMessage)

    // Continue building on the success block
    val subtypeValue = targetType.genPointerBitcast(successBlock)(supertypeValue)

    (successBlock, subtypeValue)
  }
}
