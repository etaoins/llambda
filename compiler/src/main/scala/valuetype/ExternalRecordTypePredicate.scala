package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{NativeLibrary, ProcedureSignature}

/** Represents the optional predicate for an externally defined record type
  *
  * The function must have the signature ExternalRecordTypePredicate.signature
  *
  * @param  library       Native library containing the type predicate
  * @param  nativesymbol  Native symbol for the type predicate
  */
case class ExternalRecordTypePredicate(library: NativeLibrary, nativeSymbol: String)

object ExternalRecordTypePredicate {
  val signature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    mandatoryArgTypes=List(AnySchemeType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=ReturnType.Reachable(Predicate),
    attributes=Set()
  )
}
