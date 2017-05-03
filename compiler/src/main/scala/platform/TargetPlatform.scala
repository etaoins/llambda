package io.llambda.compiler.platform
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.llvmir.DataLayout


/** Describes a target platform for code generation
  *
  * This may differ from the current platform during cross compilation
  */
case class TargetPlatform(dataLayout: DataLayout) {
  val pointerBits: Int = dataLayout.pointerAlignment.sizeBits

  def bytesForType(valueType: vt.ValueType) = valueType match {
    // Use the + 7 to make sure we round up to the next byte boundary
    case intLikeType: vt.IntLikeType => (intLikeType.bits + 7) / 8
    case vt.Float => 4
    case vt.Double => 8
    case pointerType: vt.PointerType => pointerBits / 8
  }
}
