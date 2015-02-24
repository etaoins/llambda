package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.{BoundSyntax, Primitives}
import llambda.compiler.frontend.syntax.ExpandMacro

private[frontend] object ExpandBodyDatum {
  /** Expands a datum in an outermost or body context
    *
    * This recursively expands any top-level macros and flattens any (begin)s
    *
    * @param  datum  Datum to expand
    * @return Zero or more expanded data
    */
  def apply(datum : sst.ScopedDatum)(implicit context : FrontendContext) : List[sst.ScopedDatum] =
    datum match {
      case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
        (appliedSymbol.resolveOpt, cdr) match {
          case (Some(syntax : BoundSyntax), _) =>
            // This is a macro - expand it
            val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)
            ExpandBodyDatum(expandedDatum)

          case (Some(Primitives.Begin), sst.ScopedProperList(innerExprs)) =>
            // This is a (begin) - flatten it
            innerExprs.flatMap(ExpandBodyDatum.apply)

          case _ => List(datum)
        }

      case _ => List(datum)
    }
}
