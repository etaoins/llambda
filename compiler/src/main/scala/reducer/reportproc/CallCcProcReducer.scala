package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

import annotation.tailrec

object CallCcProcReducer extends ReportProcReducer {
  private val callCcNames = Set("call-with-current-continuation", "call/cc")
  
  private def storageLocReferencedByExpr(storageLoc : StorageLocation, expr : et.Expr) : Boolean = expr match {
    case et.VarRef(refedLoc) =>
      refedLoc == storageLoc

    case nonRef =>
      nonRef.subexprs.exists(storageLocReferencedByExpr(storageLoc, _))
  }
  
  /**
   * Replaces any top-level et.Return() expressions with the returned expression and drops the rest of the body 
   *
   * This is important as et.Return() is difficult for the reducer and later passes to statically reason about
   */
  @tailrec
  private def convertUnconditionalReturn(bodyExprs : List[et.Expr], acc : List[et.Expr] = List()) : List[et.Expr] = bodyExprs match {
    case et.Return(returnedExpr) :: discaredTail =>
      // We exited!
      // Discard our tail and change from an et.Return to a normal body
      (returnedExpr :: acc).reverse

    case other :: tail =>
      convertUnconditionalReturn(tail, other :: acc)

    case Nil =>
      acc.reverse
  } 

  private def replaceExitProcWithReturn(expr : et.Expr, exitProc : StorageLocation) : et.Expr = expr match {
    case applyExpr @ et.Apply(et.VarRef(appliedVar), List(returnedExpr)) if appliedVar == exitProc =>
      // Perform replacement in the expression we're returning
      val replacedReturnedExpr = replaceExitProcWithReturn(returnedExpr, exitProc)

      et.Return(replacedReturnedExpr).assignLocationFrom(expr)

    case lambdaExpr : et.Lambda =>
      // Don't recurse inside nested lambdas
      // Calling our exit proc inside a nested lambda should return to the outer lambda but et.Return only exits
      // from the current procedure
      lambdaExpr

    case _ =>
      // Replace any uses of the exit proc in our subexprs
      expr.map(replaceExitProcWithReturn(_, exitProc))
  }

  def apply(appliedVar : ReportProcedure, operands : List[et.Expr])(implicit reduceConfig : ReduceConfig) : Option[et.Expr] = (appliedVar.reportName, operands) match {
    case (reportName, List(et.Lambda(List(exitProc), None, bodyExpr))) if callCcNames.contains(reportName) => 
      // Try to convert all uses of the exit proc in to a return
      val replacedBodyExpr = replaceExitProcWithReturn(bodyExpr, exitProc)

      // Convert any returns in the tail position back to normal Scheme last value returns
      // This is easier for the reducer and planner to reason about
      val dereturnedBodyExpr = et.Expr.fromSequence(
        convertUnconditionalReturn(replacedBodyExpr.toSequence)
      )

      val finalExpr = if (storageLocReferencedByExpr(exitProc, dereturnedBodyExpr)) {
        // We still need to invoke (call/cc) to get an exit proc value
        et.Apply(et.VarRef(appliedVar), List(et.Lambda(List(exitProc), None, dereturnedBodyExpr)))
      }
      else if (ExprCanReturn(dereturnedBodyExpr)) {
        // We converted all uses of the exit proc to et.Return. We can strip out the (call/cc) completely
        // This can be a big efficiency win
        et.Apply(et.Lambda(Nil, None, dereturnedBodyExpr), Nil)
      }
      else {
        // The (call/cc) collapsed in to a completely normal Scheme expression with no returns
        // We can use the dereturned body directly
        dereturnedBodyExpr
      }

      Some(finalExpr)

    case _ =>
      None
  }
}
