package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

/** Represents a type variable with an optional upper type bound
  *
  * This is implemented as a virtual record type representing the type variable. This allows most of typing
  * infrastructure to handle the type variable transparently before it's been resolved.
  */
class TypeVar(sourceName : String, val upperBound : SchemeType = AnySchemeType) extends RecordType(sourceName, Nil)
