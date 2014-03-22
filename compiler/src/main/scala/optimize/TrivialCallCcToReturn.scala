package io.llambda.compiler.optimize
import io.llambda

import llambda.compiler._

object TrivialCallCcToReturn extends Optimizer {
  private val callCcNames = List("call-with-current-continuation", "call/cc")

  private case class ReplacementResult(
    newExpression : et.Expression,
    requiresCallCc : Boolean
  )

  private def storageLocReferencedByExpr(storageLoc : StorageLocation, expr : et.Expression) : Boolean = expr match {
    case et.VarRef(refedLoc) =>
      refedLoc == storageLoc

    case nonRef =>
      nonRef.subexpressions.exists(storageLocReferencedByExpr(storageLoc, _))
  }

  private def replaceExitProcWithReturn(expr : et.Expression, exitProc : StorageLocation) : ReplacementResult = expr match {
    case applyExpr @ et.Apply(et.VarRef(appliedVar), List(returnedExpr)) if appliedVar == exitProc =>
      // Perform replacement in the expression we're returning
      val replaceReturnedExprResult = replaceExitProcWithReturn(returnedExpr, exitProc)

      ReplacementResult(
        newExpression=et.Return(replaceReturnedExprResult.newExpression),
        // The returned expression itself may reference the exit proc
        requiresCallCc=replaceReturnedExprResult.requiresCallCc
      )

    case lambdaExpr @ et.Lambda(_, _, bodyExpr) =>
      ReplacementResult(
        newExpression=lambdaExpr,
        // Check if the lambda captures the exit proc
        requiresCallCc=storageLocReferencedByExpr(exitProc, bodyExpr)
      )

    case et.VarRef(refedLoc) if refedLoc == exitProc=>
      ReplacementResult(
        newExpression=expr,
        // Our exit procedure was referenced besides an application 
        requiresCallCc=true
      )

    case _ =>
      var requiresCallCc = false

      // Replace any uses of the exit proc in our subexpressions
      val newExpr = expr.map { subexpression =>
        val result = replaceExitProcWithReturn(subexpression, exitProc)

        // If any of our subexpressions require a call/cc then we do
        requiresCallCc = requiresCallCc || result.requiresCallCc

        result.newExpression
      }

      ReplacementResult(
        newExpression=newExpr,
        requiresCallCc=requiresCallCc
      )
  }

  def apply(expr : et.Expression) : et.Expression = expr match {
    case et.Apply(
      et.VarRef(reportProc : ReportProcedure),
      List(et.Lambda(List(exitProc), None, bodyExpr))
    ) if callCcNames.contains(reportProc.reportName) =>
      // This is a call/cc!

      // Try to convert all uses of the exit proc in to a return
      val replacementResult = replaceExitProcWithReturn(bodyExpr, exitProc)

      // Recursively replace any other (call/cc) users inside the body
      val newBody = replacementResult.newExpression.map(apply)

      if (replacementResult.requiresCallCc) {
        // We still need to invoke (call/cc)
        et.Apply(et.VarRef(reportProc), List(et.Lambda(List(exitProc), None, newBody)))
      }
      else {
        // We can strip out the (call/cc) completely
        // This can be a big efficiency wine
        et.Apply(et.Lambda(Nil, None, newBody), Nil)
      }

    case _ =>
      expr.map(apply)
  }
}
