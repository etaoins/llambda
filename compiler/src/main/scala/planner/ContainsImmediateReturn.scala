package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et

private[planner] object ContainsImmediateReturn extends ((et.Expr) => Boolean)  {
  def apply(expr : et.Expr) : Boolean = expr match {
    case _ : et.Return =>
      true

    case lambda : et.Lambda  =>
      // If the return exists in tbe body of a nested lambda it's not immediate
      false

    case _ =>
      expr.subexprs.exists(ContainsImmediateReturn)
  }

}
