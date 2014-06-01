package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object ExprCanReturn {
  def apply(expr : et.Expr) : Boolean = expr match {
    case _ : et.Return => true
    case _ : et.Lambda => false
    case other =>
      other.subexprs.exists(ExprCanReturn(_))
  }
}
