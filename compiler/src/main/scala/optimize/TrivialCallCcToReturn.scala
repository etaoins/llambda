package io.llambda.compiler.optimize
import io.llambda

import llambda.compiler._

object TrivialCallCcToReturn extends Optimizer {
  private val callCcNames = List("call-with-current-continuation", "call/cc")
  private class ReplacementFailed extends Exception

  private def storageLocReferencedByExpr(storageLoc : StorageLocation, expr : et.Expression) : Boolean = expr match {
    case et.VarRef(refedLoc) =>
      refedLoc == storageLoc

    case nonRef =>
      nonRef.subexpressions.exists(storageLocReferencedByExpr(storageLoc, _))
  }

  private def replaceExitProcWithReturn(expr : et.Expression, exitProc : StorageLocation) : et.Expression = expr match {
    case applyExpr @ et.Apply(et.VarRef(appliedVar), List(returnedExpr)) if appliedVar == exitProc =>
      if (storageLocReferencedByExpr(exitProc, returnedExpr)) {
        // Tricky - we tried to return something depending on ourselves
        throw new ReplacementFailed
      }
      else {
        // This is a trivial call/cc usage
        et.Return(returnedExpr)
      }

    case lambdaExpr @ et.Lambda(_, _, bodyExpr) =>
      if (storageLocReferencedByExpr(exitProc, bodyExpr)) {
        // This lambda captures the exit proc
        // That is bad
        throw new ReplacementFailed
      }

      lambdaExpr

    case et.VarRef(refedLoc) if refedLoc == exitProc=>
      // The variable was referenced somehow - abort!
      throw new ReplacementFailed

    case _ =>
      // Replace any uses of the exit proc in our subexppresions
      expr.map(replaceExitProcWithReturn(_, exitProc))
  }

  def apply(expr : et.Expression) : et.Expression = expr match {
    case et.Apply(
      et.VarRef(reportProc : ReportProcedure),
      List(et.Lambda(List(exitProc), None, bodyExpr))
    ) if callCcNames.contains(reportProc.reportName) =>
      // This is a call/cc!

      // Try to convert all uses of the exit proc in to a return
      // If that succeeds we can remove the call/cc application completely
      // If the exit proc used for anything but an application a ReplacementFailed exception will be thrown
      val replacedExpr = try {
        val replacedBody = replaceExitProcWithReturn(bodyExpr, exitProc)

        et.Apply(et.Lambda(Nil, None, replacedBody), Nil)
      }
      catch {
        case _ : ReplacementFailed =>
          expr
      }

      // Now look for replacements to make in our subexpressions
      replacedExpr.map(apply)

    case _ =>
      expr.map(apply)
  }
}
