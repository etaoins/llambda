package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

object GenSizeOf {
  def apply(block : IrBlockBuilder)(irType : FirstClassType) : IrValue = {
    // Do a fake getelementptr as a contant
    val typeNullPointer = NullPointerConstant(PointerType(irType))
    val fakeElementPtr = ElementPointerConstant(irType, typeNullPointer, List(1))

    // Convert to an integer
    block.ptrtoint("sizeOfValuei")(fakeElementPtr, IntegerType(64))
  }
}

