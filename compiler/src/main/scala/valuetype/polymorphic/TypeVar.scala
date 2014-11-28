package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

/** Represents a type variable with an optional upper type bound */
case class TypeVar(upperBound : SchemeType = AnySchemeType)
