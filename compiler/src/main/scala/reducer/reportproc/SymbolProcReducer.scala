package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object SymbolProcReducer extends ReportProcReducer {
  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (reportName, operands) match {
    case ("symbol=?", _) if operands.length >= 2 =>
      val literalSymbolOperands = operands.map(LiteralForExpr(_)).collect {
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
