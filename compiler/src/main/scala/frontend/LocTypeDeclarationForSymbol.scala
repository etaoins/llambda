package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{LocTypeDeclaration, MonomorphicDeclaration}
import llambda.compiler.BadSpecialFormException

private[frontend] object LocTypeDeclarationForSymbol {
  /** Returns the storage location type declaration for a given symbol
    *
    * This uses three sources of information: previous forward type declarations in the symbol's scope, any declaration
    * provided at definition time and a default type
    *
    * @param  symbol           Symbol being defined. The symbol's scope will the examined for forward type declarations
    * @param  providedDeclOpt  Optional type declaration provided at definition time. If this conflicts with a previous
    *                          forward declaration then a BadSpecialFormException will be thrown
    * @param  defaultType      Default type to use if no type declaration can be found
    */
  def apply(
      symbol: sst.ScopedSymbol,
      providedDeclOpt: Option[LocTypeDeclaration] = None,
      defaultType: vt.SchemeType = vt.AnySchemeType
  ): LocTypeDeclaration = {
    symbol.scope.typeDeclarations.get(symbol) match {
      case Some(declaredType) =>
        // Does the declared type match the provided type exactly?
        providedDeclOpt match {
          case Some(incompatibleType) if incompatibleType != declaredType =>
            throw new BadSpecialFormException(symbol, s"Symbol previously declared with type ${declaredType}")

          case _ =>
        }

        declaredType

      case None =>
        // No type declaration
        providedDeclOpt.getOrElse(MonomorphicDeclaration(defaultType))
    }
  }
}
