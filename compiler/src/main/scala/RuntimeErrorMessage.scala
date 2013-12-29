package io.llambda.compiler
import io.llambda

/** Human-readable runtime error message
  *
  * @param  name  Internal identifier unique to the error message
  * @param  text  Human-readable error message
  */
case class RuntimeErrorMessage(
  name : String,
  text : String
)

