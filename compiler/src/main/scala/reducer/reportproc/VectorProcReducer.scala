package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._
import io.llambda.compiler.reducer.{partialvalue => pv}

object VectorProcReducer extends ReportProcReducer {
  def apply(appliedVar : ReportProcedure, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (appliedVar.reportName, operands) match {
    case ("vector?", List(singleExpr)) =>
      PartialValueForExpr(singleExpr).flatMap {
        case _ : pv.PartialVector => 
          Some(et.Literal(ast.BooleanLiteral(true)))
        
        case _ : pv.PartialPair => 
          Some(et.Literal(ast.BooleanLiteral(false)))

        case _ : pv.LiteralLeaf => 
          Some(et.Literal(ast.BooleanLiteral(false)))

        case _ : pv.ReducedExpr =>
          // This could be anything
          None
      }

    case ("vector-length", List(singleExpr)) =>
      PartialValueForExpr(singleExpr) match {
        case Some(pv.PartialVector(elems)) =>
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
