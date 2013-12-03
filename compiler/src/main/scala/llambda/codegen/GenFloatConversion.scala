package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{valuetype => vt}

import llambda.InternalCompilerErrorException

object GenFloatConversion {
  def apply(block : IrBlockBuilder)(fromValue : IrValue, fpType : vt.FpType) : IrValue = fpType match {
    // This works because we only have two FP types
    case vt.Double =>
      block.fpextTo("fpexted")(fromValue, DoubleType)
    case vt.Float =>
      block.fptruncTo("fpexted")(fromValue, FloatType)
  }
}


