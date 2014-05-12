package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object EquivalenceProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case (reportName, List(val1Expr, val2Expr)) if List("eqv?", "eq?").contains(reportName) =>
      for(val1 <- LiteralForExpression(val1Expr);
          val2 <- LiteralForExpression(val2Expr)) {
        return LiteralEqv.literalsAreEqv(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      }

      None
    
    case ("equal?", List(val1Expr, val2Expr)) =>
      for(val1 <- LiteralForExpression(val1Expr);
          val2 <- LiteralForExpression(val2Expr)) {
        return LiteralEqv.literalsAreEqual(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      } 

      None

    case _ =>
      None
  }
}
