package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.valuetype.{polymorphic => pm}

object ExtractTypeVar extends (sst.ScopedDatum => (sst.ScopedSymbol, pm.TypeVar)) {
  /** Parsed a type variable returning its name a variable definition
    *
    * This accepts either a bare identifier or [identifier : <upper-bound>]
    */
  def apply(datum : sst.ScopedDatum) : (sst.ScopedSymbol, pm.TypeVar) = datum match {
    case sst.ScopedProperList(List(
      argName : sst.ScopedSymbol,
      sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
      upperBoundDatum : sst.ScopedDatum
    )) =>
      // An upper type bound was supplied
      val upperBound = ExtractType.extractSchemeType(upperBoundDatum)
      (argName -> new pm.TypeVar(argName.name, upperBound))

    case argName : sst.ScopedSymbol =>
      (argName -> new pm.TypeVar(argName.name))

    case other =>
      val message = s"Unrecognized type variable definition. Must be either identiifer or [identifier : <upper-bound>]."
      throw new BadSpecialFormException(other, message)
  }
}
