package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object StringProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("string?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.StringLiteral]
      })

    case ("string-length", List(singleExpr)) =>
      LiteralValue(singleExpr) match {
        case Some(ast.StringLiteral(stringValue)) =>
          Some(et.Literal(ast.IntegerLiteral(
            stringValue.length
          )))

        case _ => 
          None
      }
    
    // Make sure we only convert strings that are directly passed
    // This prevents duplicating data between a possibly reused string and the constant symbol we create
    // Also note we intentionally don't reduce symbol->string as our stdlib always creates mutable strings and we
    // don't want to do things semantically different from the stdlib. Symbols are always immutable so the below is
    // safe
    case ("string->symbol", List(et.Literal(ast.StringLiteral(stringValue)))) =>
      Some(et.Literal(
        ast.Symbol(stringValue)
      ))

    case _ =>
      None
  }
}
