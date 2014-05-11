package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object BooleanProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("boolean?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.BooleanLiteral]
      })

    case ("not", List(singleExpr)) =>
      LiteralForExpression(singleExpr) map {
        // Everything but false is truth-y
        case ast.BooleanLiteral(false) =>
          et.Literal(ast.BooleanLiteral(true))

        case _ =>
          et.Literal(ast.BooleanLiteral(false))
      }

    case ("boolean=?", _) if operands.length >= 2 =>
      val literalBoolOperands = operands.map(LiteralForExpression(_)).collect {
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
