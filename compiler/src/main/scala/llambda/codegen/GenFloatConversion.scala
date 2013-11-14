package llambda.codegen

import llambda.codegen.llvmir._
import llambda.nfi

import llambda.InternalCompilerErrorException

object GenFloatConversion {
  def apply(block : IrBlockBuilder)(fromValue : IrValue, fpType : nfi.FpType) : IrValue = fpType match {
    // This works because we only have two FP types
    case nfi.Double =>
      block.fpextTo("fpexted")(fromValue, DoubleType)
    case nfi.Float =>
      block.fptruncTo("fpexted")(fromValue, FloatType)
  }
}


