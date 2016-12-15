package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.sst
import llambda.compiler.{SourceLocated, BadSpecialFormException, Primitives}

private[frontend] object ExtractExternalRecordType {
  def apply(
      located: SourceLocated,
      sourceNameOpt: Option[String],
      constructorArgs: List[sst.ScopedDatum]
  ): vt.ExternalRecordType =
    constructorArgs match {
      case Nil =>
        new vt.ExternalRecordType(sourceNameOpt, None)

      case List(predicateDef @ sst.ProperList(sst.ResolvedSymbol(Primitives.NativeFunction) :: predicateArgs)) =>
        val predicateFunction = ExtractNativeFunction(predicateDef, hasWorldArg=false, predicateArgs)

        if (predicateFunction.polySignature != vt.ExternalRecordTypePredicate.signature.toPolymorphic) {
          throw new BadSpecialFormException(predicateDef, "External record type predicates must have type (-> <any> <native-bool>)")
        }

        new vt.ExternalRecordType(sourceNameOpt, Some(
          vt.ExternalRecordTypePredicate(predicateFunction.library, predicateFunction.nativeSymbol)
        ))

      case _ =>
        throw new BadSpecialFormException(located, "ExternalRecord takes an optional predicate (native-function)")
    }
}
