package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object LiteralForExpr {
  def apply(expr : et.Expr, allowImpureExprs : Boolean = false)(implicit reduceConfig : ReduceConfig) : Option[ast.Datum] =
    PartialValueForExpr(expr, allowImpureExprs).flatMap(_.toDatumOpt)
}
