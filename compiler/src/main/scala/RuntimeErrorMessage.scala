package io.llambda.compiler


/** Human-readable runtime error message
  *
  * @param  category  High-level category for the error
  * @param  text      Human-readable error message
  */
case class RuntimeErrorMessage(category: ErrorCategory, text: String)
