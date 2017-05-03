package io.llambda.compiler.platform
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.llvmir.DataLayout


/** Describes a target platform for code generation
  *
  * This may differ from the current platform during cross compilation
  */
case class TargetPlatform(dataLayout: DataLayout) {
  val pointerBits: Int = dataLayout.pointerLayout.sizeBits
}
