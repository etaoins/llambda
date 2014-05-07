package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object SymbolProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("symbol?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.Symbol]
      })
    
    case ("symbol=?", _) if operands.length >= 2 =>
      val literalSymbolOperands = operands.map(LiteralValue(_)).collect {
        case Some(ast.Symbol(symbol)) =>
          symbol
      }

      if (literalSymbolOperands.length == operands.length) {
        Some(et.Literal(ast.BooleanLiteral(
          literalSymbolOperands.forall(_ == literalSymbolOperands.head)
        )))
      }
      else {
        None
      }

    case _ =>
      None
  }
}
