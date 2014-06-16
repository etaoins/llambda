package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object CompactRepresentationForType {
  /** Returns the most compact representation for the specified type
    *
    * Compact in this context refers to both the memory and garbage collector overhead of the type. This is typically an
    * unboxed value for types that have unboxed representations
    */
  def apply(valueType : vt.ValueType) : vt.ValueType = valueType match {
    case vt.IntrinsicCellType(ct.ExactIntegerCell) => vt.Int64
    case vt.IntrinsicCellType(ct.InexactRationalCell) => vt.Double
    case vt.IntrinsicCellType(ct.BooleanCell) => vt.CBool
    case vt.IntrinsicCellType(ct.CharacterCell) => vt.UnicodeChar
    case other => other
  }
}
