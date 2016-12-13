package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

object GenSizeOf {
  /** Generates a constant representing the size of the passed type in bytes
    *
    * @param  irType      Type to extract the size from
    * @param  resultType  Type of the resulting constant. This should be large enough to represent the size of the type.
    * @return Constant size of the type in bytes
    */
  def apply(irType: FirstClassType, resultType: IntegerType = IntegerType(64)): IrConstant = {
    // Do a fake getelementptr as a contant
    val typeNullPointer = NullPointerConstant(PointerType(irType))
    val fakeElementPtr = ElementPointerConstant(irType, typeNullPointer, List(1))

    // Convert to an integer
    PtrToIntConstant(fakeElementPtr, resultType)
  }
}

