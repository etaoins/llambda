package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.codegen.llvmir._

object GenLlvmExpect {
  def apply(block : IrBlockBuilder)(value : IrValue, expectation : IrConstant) : IrValue = {
    // This is declared in defines.ll for th types we care about
    val llvmExpectIntrinsic = IrFunctionDecl(
      result=IrFunction.Result(value.irType),
      name=s"llvm.expect.${value.irType}",
      arguments=List(IrFunction.Argument(value.irType), IrFunction.Argument(value.irType))
    )

    block.callDecl(Some("expectedValue"))(llvmExpectIntrinsic, List(value, expectation)).get
  }
}
