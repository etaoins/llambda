package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object EquivalenceProcReducer extends ReportProcReducer {
  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (reportName, operands) match {
    case (reportName, List(val1Expr, val2Expr)) if List("eqv?", "eq?").contains(reportName) =>
      for(val1 <- LiteralForExpr(val1Expr);
          val2 <- LiteralForExpr(val2Expr)) {
        return LiteralEqv.literalsAreEqv(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      }

      None
    
    case ("equal?", List(val1Expr, val2Expr)) =>
      for(val1 <- LiteralForExpr(val1Expr);
          val2 <- LiteralForExpr(val2Expr)) {
        return LiteralEqv.literalsAreEqual(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      } 

      None

    case _ =>
      None
  }
}
