package io.llambda.compiler


/** Human-readable runtime error message
  *
  * @param  category  High-level category for the error
  * @param  name      Internal identifier unique to the error message
  * @param  text      Human-readable error message
  */
case class RuntimeErrorMessage(category: ErrorCategory, name: String, text: String)
