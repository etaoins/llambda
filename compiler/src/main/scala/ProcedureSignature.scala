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
  fixedArgTypes : List[vt.ValueType],
  restArgMemberTypeOpt : Option[vt.SchemeType],
  returnType : vt.ReturnType.ReturnType[vt.ValueType],
  attributes : Set[ProcedureAttribute]
) {
  /** Converts this signature to a polymorphic signaure with no type variables */
  def toPolymorphic =
    PolymorphicSignature(Set(), this)

  def toSchemeProcedureType = {
    val schemeReturnType = if (attributes.contains(ProcedureAttribute.NoReturn)) {
      vt.ReturnType.SingleValue(vt.EmptySchemeType)
    }
    else {
      returnType.schemeReturnType
    }

    vt.ProcedureType(
      fixedArgTypes=fixedArgTypes.map(_.schemeType),
      restArgMemberTypeOpt=restArgMemberTypeOpt,
      returnType=schemeReturnType
    )
  }
}
