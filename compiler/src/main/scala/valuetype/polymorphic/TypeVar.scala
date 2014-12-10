package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

/** Represents a type variable with an optional upper type bound
  *
  * This is implemented as a virtual record type representing the type variable. This allows most of typing
  * infrastructure to handle the type variable transparently before it's been resolved.
  */
class TypeVar(
    val sourceName : String,
    val upperBound : SchemeType = AnySchemeType
) extends RecordTypeInstance(ReconcileTypeVars.Result(), new RecordType(sourceName, Nil))

/** Represents a type variable being used in an illegal context
  *
  * If ExtractType encounters a poison type var it will raise a BadSpecialFormException with the specified message. This
  * is used to disallow mutable polymorphic record fields, for example.
  */
class PoisonTypeVar(
    val message : String
) extends TypeVar("!poison!")
