package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir
import llambda.compiler.{valuetype => vt}

case class SignedFirstClassType(
  irType: llvmir.FirstClassType,
  signed: Option[Boolean])

object ValueTypeToIr {
  def apply(valueType: vt.ValueType): SignedFirstClassType = valueType match {
    case intLike: vt.IntLikeType =>
      SignedFirstClassType(llvmir.IntegerType(intLike.bits), Some(intLike.signed))

    case vt.Float =>
      SignedFirstClassType(llvmir.FloatType, None)

    case vt.Double =>
      SignedFirstClassType(llvmir.DoubleType, None)

    case cellValueType: vt.CellValueType =>
      SignedFirstClassType(llvmir.PointerType(cellValueType.cellType.irType), None)
  }
}
