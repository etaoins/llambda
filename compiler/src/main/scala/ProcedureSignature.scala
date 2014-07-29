package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

object ProcedureAttribute {
  sealed abstract class ProcedureAttribute

  /** Indicates a procedure cannot return normally
    *
    * This is typically used for functions that raise exceptions or terminate the program
    */
  case object NoReturn extends ProcedureAttribute

  /** Indicates that a fast, potentially non-interoperable calling convention should be used for this procedure 
    *
    * This must not be used for procedures that can be directly invoked from C++ such as trampolines
   */
  case object FastCC extends ProcedureAttribute
}

/** Describes the signature of an invokable function
  *
  * This includes both native functions and generated lambdas 
  */
case class ProcedureSignature(
  hasWorldArg : Boolean,
  hasSelfArg : Boolean,
  fixedArgs : List[vt.ValueType],
  restArgOpt : Option[vt.SchemeType],
  returnType : Option[vt.ValueType],
  attributes : Set[ProcedureAttribute.ProcedureAttribute]
)
