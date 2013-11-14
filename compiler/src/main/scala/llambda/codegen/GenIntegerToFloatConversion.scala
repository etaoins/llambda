package llambda.codegen

import llambda.codegen.llvmir._
import llambda.nfi

import llambda.InternalCompilerErrorException

object GenIntegerToFloatConversion {
  def apply(block : IrBlockBuilder)(fromValue : IrValue, fromSigned : Boolean, fpType : nfi.FpType) : IrValue = {
    val convFunction = if (fromSigned) {
      block.sitofp
    }
    else {
      block.uitofp
    }
    
    // Convert to floating point
    fpType match {
      case nfi.Float =>
        convFunction("floatConv")(fromValue, FloatType)
      case nfi.Double =>
        convFunction("floatConv")(fromValue, DoubleType)
    }
  }
}
