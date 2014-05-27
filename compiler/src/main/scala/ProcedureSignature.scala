package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

sealed abstract class ProcedureAttribute
object ProcedureAttribute {
  /** Indicates a procedure cannot return normally
    *
    * This is typically used for functions that raise exceptions or terminate the program
    */
  case object NoReturn extends ProcedureAttribute
}

/** Describes the signature of an invokable function
  *
  * This includes both native functions and generated lambdas 
  */
case class ProcedureSignature(
  hasWorldArg : Boolean,
  hasSelfArg : Boolean,
  fixedArgs : List[vt.ValueType],
  hasRestArg : Boolean,
  returnType : Option[vt.ValueType],
  attributes : Set[ProcedureAttribute]
)
