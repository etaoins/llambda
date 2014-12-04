package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Value field of mutables */
case class MutableField(innerType : vt.ValueType) extends vt.RecordField("value", innerType)

/** Mutable values are implemented as single field records */
case class MutableType(innerType : vt.ValueType) extends {
  val recordField = MutableField(innerType)
} with vt.RecordType("mutable" + vt.NameForType(innerType), List(recordField))
