package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

trait ReportProcReducer {
  protected def literalPredicate(expr : et.Expr, predicate : (ast.Datum) => Boolean)(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = {
    LiteralForExpr(expr).map { exprLiteral =>
      val predicateValue = predicate(exprLiteral)
      et.Literal(ast.BooleanLiteral(predicateValue))
    }
  }

  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] 
}
