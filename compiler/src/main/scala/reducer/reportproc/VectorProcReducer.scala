package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object VectorProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("vector?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.VectorLiteral]
      })

    case ("vector-length", List(singleExpr)) =>
      LiteralValue(singleExpr) match {
        case Some(ast.VectorLiteral(elems)) =>
          Some(et.Literal(ast.IntegerLiteral(
            elems.length
          )))

        case _ => 
          None
      }

    case _ =>
      None
  }
}
