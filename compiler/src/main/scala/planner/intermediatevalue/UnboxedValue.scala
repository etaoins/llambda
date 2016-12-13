package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}

/** Common trait for values that can directly produce an unboxed native representation
  *
  * This is implemented by NativeValue to represent dynamic unboxed values and by the relevant ConstantValue types
  */
abstract trait UnboxedValue extends IntermediateValue {
  val nativeType: vt.NativeType

  override def preferredRepresentation: vt.ValueType =
    nativeType
}
