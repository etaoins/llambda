package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object ListProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("null?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.EmptyList]
      })
    
    case ("pair?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.Pair]
      })
    
    case ("length", List(singleExpr)) =>
      LiteralValue(singleExpr) flatMap {
        case ast.ProperList(elements) =>
          Some(et.Literal(ast.IntegerLiteral(elements.length)))

        case _ =>
          None
      }
    
    case _ => 
      None
  }
}
