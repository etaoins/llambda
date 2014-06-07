package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._
import io.llambda.compiler.reducer.{partialvalue => pv}

object ApplyProcReducer extends ReportProcReducer {
  def apply(appliedVar : et.VarRef, reportName : String, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (reportName, operands) match {
    // This doesn't handle apply with > 2 arguments
    case ("apply", List(appliedExpr, operandListExpr)) => 
      PartialValueForExpr(operandListExpr) match {
        case Some(pv.ProperList(operandPartialValues)) =>
          // These are guaranteed to be pre-reduced
          val reducedOperands = operandPartialValues.flatMap(_.toExprOpt)

          if (reducedOperands.length != operandPartialValues.length) {
            // Couldn't convert all arguments to expressions
            None
          }
          else {
            ReduceApplication(appliedExpr, reducedOperands) orElse {
              // We can at least turn this to a normal application
              Some(et.Apply(appliedExpr, reducedOperands))
            }
          }

        case _ =>
          None
      }

    case _ =>
      None
  }
}
