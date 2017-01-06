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

  /** Indicates that the procedure captures none of its pointer arguments
    *
    * This means it cannot save, return, raise, or place the pointer inside another argument. This applies recursively;
    * any pointers inside the argument must not be captured.
    */
  case object NoCapture extends ProcedureAttribute
}

/** Describes the signature of an invokable function
  *
  * This includes both native functions and generated lambdas
  */
case class ProcedureSignature(
  hasWorldArg: Boolean,
  hasSelfArg: Boolean,
  mandatoryArgTypes: List[vt.ValueType],
  optionalArgTypes: List[vt.SchemeType],
  restArgMemberTypeOpt: Option[vt.SchemeType],
  returnType: vt.ReturnType.ReturnType[vt.ValueType],
  attributes: Set[ProcedureAttribute]
) {
  /** Converts this signature to a polymorphic signaure with no type variables */
  def toPolymorphic =
    PolymorphicSignature(Set(), this)

  def toSchemeProcedureType = {
    val schemeReturnType = if (attributes.contains(ProcedureAttribute.NoReturn)) {
      vt.ReturnType.Reachable(vt.EmptySchemeType)
    }
    else {
      returnType.schemeReturnType
    }

    vt.ProcedureType(
      mandatoryArgTypes=mandatoryArgTypes.map(_.schemeType),
      optionalArgTypes=optionalArgTypes,
      restArgMemberTypeOpt=restArgMemberTypeOpt,
      returnType=schemeReturnType
    )
  }
}
