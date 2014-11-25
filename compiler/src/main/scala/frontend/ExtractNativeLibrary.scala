package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{ast, sst, NativeLibrary, NativeStaticLibrary, Primitives}
import llambda.compiler.BadSpecialFormException

object ExtractNativeLibrary {
  def apply(datum : sst.ScopedDatum) : NativeLibrary = datum match {
    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case nativeLibrary : NativeLibrary =>
          nativeLibrary

        case _ =>
          throw new BadSpecialFormException(symbol, "Other value used where native library expected")
      }

    case sst.ScopedProperList(List(staticLibrary : sst.ScopedSymbol, sst.NonSymbolLeaf(ast.StringLiteral(libraryName))))
        if staticLibrary.resolve == Primitives.StaticLibrary =>
      NativeStaticLibrary(libraryName)

    case _ =>
      throw new BadSpecialFormException(datum, "Bad native library definition")
  }
}
