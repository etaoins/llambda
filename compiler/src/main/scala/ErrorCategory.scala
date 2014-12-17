package io.llambda.compiler
import io.llambda

/** Represents a category of error that can be raised at runtime
  *
  * See runtime/binding/ErrorCategory.h for a description of each category
  *
  * @param  runtimeId  Numerical ID for this category as defined in runtime/binding/ErrorCategory.h
  */
sealed abstract class ErrorCategory(val runtimeId : Int)

object ErrorCategory {
  object Default            extends ErrorCategory(0)
  object File               extends ErrorCategory(1)
  object Read               extends ErrorCategory(2)
  object Type               extends ErrorCategory(3)
  object Arity              extends ErrorCategory(4)
  object Range              extends ErrorCategory(5)
  object Utf8               extends ErrorCategory(6)
  object DivideByZero       extends ErrorCategory(7)
  object MutateLiteral      extends ErrorCategory(8)
  object UndefinedVariable  extends ErrorCategory(9)
}