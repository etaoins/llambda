package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{valuetype => vt}

object CompactRepresentationForType {
  /** Returns the most compact representation for the specified type
    *
    * Compact in this context refers to both the memory and garbage collector overhead of the type. This is typically an
    * unboxed value for types that have unboxed representations
    */
  def apply(valueType : vt.ValueType) : vt.ValueType = valueType match {
    case vt.ExactIntegerType => vt.Int64
    case vt.InexactRationalType => vt.Double
    case vt.BooleanType => vt.CBool
    case vt.CharacterType => vt.UnicodeChar
    case other => other
  }
}
