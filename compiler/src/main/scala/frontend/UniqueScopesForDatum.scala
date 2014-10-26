package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.Scope
import llambda.compiler.sst

/** Returns a list of distinct scopes inside a scoped datum and its children */
object UniqueScopesForDatum extends (sst.ScopedDatum => Set[Scope]) {
  def apply(datum : sst.ScopedDatum) : Set[Scope] = datum match {
    case sst.ScopedPair(car, cdr) =>
      UniqueScopesForDatum(car) ++ UniqueScopesForDatum(cdr)

    case sst.ScopedSymbol(scope, name) =>
      Set(scope)

    case sst.ScopedVectorLiteral(elements) =>
      elements.foldLeft(Set[Scope]()) { (scopes, element) =>
        scopes ++ UniqueScopesForDatum(element)
      }

    case leaf : sst.NonSymbolLeaf =>
      Set()
  }
}
