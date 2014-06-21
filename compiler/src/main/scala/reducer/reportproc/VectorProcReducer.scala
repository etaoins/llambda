package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._
import io.llambda.compiler.reducer.{partialvalue => pv}

object VectorProcReducer extends ReportProcReducer {
  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (reportName, operands) match {
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
