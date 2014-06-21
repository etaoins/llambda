package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object BooleanProcReducer extends ReportProcReducer {
  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (reportName, operands) match {
    case ("not", List(singleExpr)) =>
      LiteralForExpr(singleExpr) map {
        // Everything but false is truth-y
        case ast.BooleanLiteral(false) =>
          et.Literal(ast.BooleanLiteral(true))

        case _ =>
          et.Literal(ast.BooleanLiteral(false))
      }

    case ("boolean=?", _) if operands.length >= 2 =>
      val literalBoolOperands = operands.map(LiteralForExpr(_)).collect {
        case Some(ast.BooleanLiteral(boolValue)) =>
          boolValue
      }

      if (literalBoolOperands.length == operands.length) {
        Some(et.Literal(ast.BooleanLiteral(
          literalBoolOperands.forall(_ == literalBoolOperands.head)
        )))
      }
      else {
        None
      }

    case _ => 
      None
  }
}
