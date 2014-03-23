package io.llambda.compiler.optimize
import io.llambda

import llambda.compiler._

object TrivialCallCcToReturn extends Optimizer {
  private val callCcNames = List("call-with-current-continuation", "call/cc")

  private def storageLocReferencedByExpr(storageLoc : StorageLocation, expr : et.Expression) : Boolean = expr match {
    case et.VarRef(refedLoc) =>
      refedLoc == storageLoc

    case nonRef =>
      nonRef.subexpressions.exists(storageLocReferencedByExpr(storageLoc, _))
  }

  private def replaceExitProcWithReturn(expr : et.Expression, exitProc : StorageLocation) : et.Expression = expr match {
    case applyExpr @ et.Apply(et.VarRef(appliedVar), List(returnedExpr)) if appliedVar == exitProc =>
      // Perform replacement in the expression we're returning
      val replacedReturnedExpr = replaceExitProcWithReturn(returnedExpr, exitProc)

      et.Return(replacedReturnedExpr)

    case lambdaExpr : et.Lambda =>
      // Don't recurse inside nested lambdas
      // Calling the exit proc inside a nested lambda should return to the outer lambda but et.Return only exits
      // from the current procedure
      lambdaExpr

    case _ =>
      // Replace any uses of the exit proc in our subexpressions
      expr.map(replaceExitProcWithReturn(_, exitProc))
  }

  def apply(expr : et.Expression) : et.Expression = expr match {
    case et.Apply(
      et.VarRef(reportProc : ReportProcedure),
      List(et.Lambda(List(exitProc), None, bodyExpr))
    ) if callCcNames.contains(reportProc.reportName) =>
      // This is a call/cc!

      // Try to convert all uses of the exit proc in to a return
      val replacedBody = replaceExitProcWithReturn(bodyExpr, exitProc)

      // Recursively replace any other (call/cc) users inside the body
      val recursedBody = replacedBody.map(apply)

      if (storageLocReferencedByExpr(exitProc, recursedBody)) {
        // We still need to invoke (call/cc) to get an exit proc value
        et.Apply(et.VarRef(reportProc), List(et.Lambda(List(exitProc), None, recursedBody)))
      }
      else {
        // We converted all uses of the exit proc to et.Return. We can strip out the (call/cc) completely
        // This can be a big efficiency win
        et.Apply(et.Lambda(Nil, None, recursedBody), Nil)
      }

    case _ =>
      expr.map(apply)
  }
}
