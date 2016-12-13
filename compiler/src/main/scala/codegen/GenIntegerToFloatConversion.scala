package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}

object GenIntegerToFloatConversion {
  def apply(block: IrBlockBuilder)(fromValue: IrValue, fromSigned: Boolean, fpType: vt.FpType): IrValue = {
    val convFunction = if (fromSigned) {
      block.sitofp
    }
    else {
      block.uitofp
    }

    // Convert to floating point
    fpType match {
      case vt.Float =>
        convFunction("floatConv")(fromValue, FloatType)
      case vt.Double =>
        convFunction("floatConv")(fromValue, DoubleType)
    }
  }
}
