package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object ExpressionCanReturn {
  def apply(expr : et.Expression) : Boolean = expr match {
    case _ : et.Return => true
    case _ : et.Lambda => false
    case other =>
      other.subexpressions.exists(ExpressionCanReturn(_))
  }
}
