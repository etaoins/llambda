package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

object GenCheckedIntegerInstr {
  def apply(state: GenerationState)(
      worldPtrIr: IrValue,
      step: ps.CheckedIntegerStep,
      opName: String,
      val1: IrValue,
      val2: IrValue
  ): (GenerationState, IrValue) = {
    val entryBlock = state.currentBlock
    val irFunction = entryBlock.function
    val module = irFunction.module

    // Declare the magic LLVM builtin for overflow checking
    val builtinFunctionDecl = IrFunctionDecl(
      result=IrFunction.Result(StructureType(List(IntegerType(64), IntegerType(1)))),
      name=s"llvm.s${opName}.with.overflow.i64",
      arguments=List(
        IrFunction.Argument(IntegerType(64)),
        IrFunction.Argument(IntegerType(64))
      )
    )

    module.unlessDeclared(builtinFunctionDecl) {
      module.declareFunction(builtinFunctionDecl)
    }

    val successBlock = irFunction.startChildBlock(s"${opName}Success")
    val overflowBlock = irFunction.startChildBlock(s"${opName}Overflow")

    // Perform the operation using LLVM's overflow checking builtin
    val resultAgg = entryBlock.callDecl(Some(s"${opName}Result"))(builtinFunctionDecl, List(val1, val2)).get
    val overflowIr = entryBlock.extractvalue("overflow")(IntegerType(1), resultAgg, List(1))
    entryBlock.condBranch(overflowIr, overflowBlock, successBlock)

    // In the success block extract the final value
    val valIr = successBlock.extractvalue("intValue")(IntegerType(64), resultAgg, List(0))

    // In the overflow block raise an error
    GenErrorSignal(state.copy(currentBlock=overflowBlock))(worldPtrIr, step.overflowMessage, locatedOpt=Some(step))

    // Return the new value
    (state.copy(currentBlock=successBlock), valIr)
  }
}
