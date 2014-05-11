package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

trait ReportProcReducer {
  protected def literalPredicate(expr : et.Expression, predicate : (ast.Datum) => Boolean)(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = {
    LiteralForExpression(expr).map { exprLiteral =>
      val predicateValue = predicate(exprLiteral)
      et.Literal(ast.BooleanLiteral(predicateValue))
    }
  }

  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] 
}
