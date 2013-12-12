package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.codegen.llvmir._

import llambda.compiler.InternalCompilerErrorException

object GenIntegerConversion {
  def apply(block : IrBlockBuilder)(fromValue : IrValue, toBits : Int, signed : Boolean) : IrValue = {
    val currentType = fromValue.irType match {
      case intType : IntegerType => intType
      case _ =>
        throw new InternalCompilerErrorException("Attempted to integer convert non-integer")
    }

    val destIrType = IntegerType(toBits)

    if (toBits > currentType.bits) {
      // Extend ourselves to the dest type's width
      if (signed) {
        block.sextTo("sextedInt")(fromValue, destIrType)
      }
      else {
        block.zextTo("zextedInt")(fromValue, destIrType)
      }
    }
    else if (toBits < currentType.bits) {
      // Truncate ourselves down
      block.truncTo("truncedint")(fromValue, destIrType)
    } else {
      // We're already the right width
      fromValue
    }
  }
}

