package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.et
import llambda.compiler.ast
import llambda.compiler.sst
import llambda.compiler.Scope

object ExtractModuleBody {
  /** Converts the body of a module (library or program) to a list of expressions
    *
    * @param  data       Scheme date of the module's body
    * @param  evalScope  Scope to evaluate the body in
    */
  def apply(data : List[ast.Datum], evalScope : Scope)(implicit context : FrontendContext) : List[et.Expr] =
    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      ExpandBodyDatum(scopedDatum).flatMap {
        ExtractExpr(_, true).toSequence
      }
    }
}
