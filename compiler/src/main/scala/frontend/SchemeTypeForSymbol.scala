package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.{valuetype => vt}
import llambda.compiler.MonomorphicDeclaration

private[frontend] object SchemeTypeForSymbol {
  /** Returns the Scheme type for a given symbol
    *
    * This is a simple wrapper around LocTypeDeclarationForSymbol for users that don't need to handle polymorphic type
    * declarations.
    */
  def apply(
      symbol: sst.Symbol,
      providedTypeOpt: Option[vt.SchemeType] = None,
      defaultType: vt.SchemeType = vt.AnySchemeType
  ): vt.SchemeType =
    LocTypeDeclarationForSymbol(symbol, providedTypeOpt.map(MonomorphicDeclaration), defaultType).toSchemeType
}
