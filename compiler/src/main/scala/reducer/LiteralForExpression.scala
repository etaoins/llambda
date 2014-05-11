package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object LiteralForExpression {
  def apply(expr : et.Expression, allowImpureExprs : Boolean = false)(implicit reduceConfig : ReduceConfig) : Option[ast.Datum] =
    PartialValueForExpression(expr, allowImpureExprs).flatMap(_.toDatumOpt)
}
