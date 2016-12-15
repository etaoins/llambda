package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{ast, sst, NativeLibrary, NativeStaticLibrary, Primitives}
import llambda.compiler.BadSpecialFormException

object ExtractNativeLibrary {
  def apply(datum: sst.ScopedDatum): NativeLibrary = datum match {
    case symbol: sst.Symbol =>
      symbol.resolve match {
        case nativeLibrary: NativeLibrary =>
          nativeLibrary

        case _ =>
          throw new BadSpecialFormException(symbol, "Other value used where native library expected")
      }

    case sst.ProperList(List(
        sst.ResolvedSymbol(Primitives.StaticLibrary),
        sst.NonSymbolLeaf(ast.String(libraryName))
    )) =>
      NativeStaticLibrary(libraryName)

    case _ =>
      throw new BadSpecialFormException(datum, "Bad native library definition")
  }
}
