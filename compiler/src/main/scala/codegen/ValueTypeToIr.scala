package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

case class SignedFirstClassType(
  irType : llvmir.FirstClassType,
  signed : Option[Boolean])

object ValueTypeToIr {
  def apply(valueType : vt.ValueType) : SignedFirstClassType = valueType match {
    case intLike : vt.IntLikeType =>
      SignedFirstClassType(llvmir.IntegerType(intLike.bits), Some(intLike.signed))

    case vt.Float =>
      SignedFirstClassType(llvmir.FloatType, None)

    case vt.Double =>
      SignedFirstClassType(llvmir.DoubleType, None)

    case vt.IntrinsicCellType(cellType) =>
      SignedFirstClassType(llvmir.PointerType(cellType.irType), None)

    case recordLike : vt.RecordLikeType =>
      // Record-likes have the same IR type as their underlying cell. Their data
      // pointer is cast to the  correct type on demand
      apply(vt.IntrinsicCellType(recordLike.cellType))
  }
}
