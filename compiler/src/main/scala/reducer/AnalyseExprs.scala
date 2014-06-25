package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.{StorageLocation, et}
import llambda.compiler.InternalCompilerErrorException

case class AnalysedExprs(
  usedTopLevelExprs : List[et.Expr] = List(),
  mutableVars : Set[StorageLocation] = Set(),
  constantTopLevelBindings : List[(StorageLocation, et.Expr)] = Nil,
  usedVars : Set[StorageLocation] = Set()
)

object AnalyseExprs  {
  private def handleNestedExpr(expr : et.Expr, acc : AnalysedExprs) : AnalysedExprs = expr match {
    case _ : et.TopLevelDefine =>
      // This voids our warranty
      throw new InternalCompilerErrorException("Top-level definition found nested in other expression")

    case et.MutateVar(storageLoc, valueExpr) =>
      // Recurse down our initialiser
      val withValueAcc = handleNestedExpr(valueExpr, acc)

      // Record our mutableness
      withValueAcc.copy(
        mutableVars=withValueAcc.mutableVars + storageLoc
      )

    case et.VarRef(storageLoc) =>
      // We're used!
      acc.copy(
        usedVars=acc.usedVars + storageLoc
      )

    case other =>
      // Iterate over our subexprs
      other.subexprs.foldLeft(acc) { case (previousAcc, expr) =>
        handleNestedExpr(expr, previousAcc)
      }
  }

  private def handleTopLevelExpr(expr : et.Expr, acc : AnalysedExprs) : AnalysedExprs = expr match {
    case et.TopLevelDefine(bindings) =>
      // Filter out the bindings that are unused
      val usedBindings = bindings.filter { case (storageLoc, initialiser) =>
        TopLevelDefineRequired(storageLoc, initialiser, acc)
      }

      if (usedBindings.isEmpty) {
        // Nothing to do!
        // Drop us completely
        acc
      }
      else {
        // Create a new top-level define with just the used bindings
        val onlyUsedDefine = et.TopLevelDefine(usedBindings).assignLocationFrom(expr)

        // Add it to the list of used expressions
        val accWithOnlyUsedDefine = acc.copy(
          usedTopLevelExprs=onlyUsedDefine :: acc.usedTopLevelExprs
        )

        // Deal with our initialisers
        // Use foldRight because our exprs are being processed in reverse order
        usedBindings.foldRight(accWithOnlyUsedDefine) { case ((storageLoc, initialiser), previousAcc) =>
          val accWithBinding = if (previousAcc.mutableVars.contains(storageLoc)) {
            // Don't record the initialisers for mutable top-level bindings
            // They provide no actionable information on the value of the storage location
            previousAcc
          }
          else {
            // Record our initialiser
            previousAcc.copy(
              constantTopLevelBindings=(storageLoc -> initialiser) :: previousAcc.constantTopLevelBindings
            )
          }
          
          // Recurse down our initialiser
          handleNestedExpr(initialiser, accWithBinding)
        }
      }

    case _ =>
      val withExprAcc = handleNestedExpr(expr, acc)

      // This is a non-(define)
      // Leave it in the used expression list
      withExprAcc.copy(
        usedTopLevelExprs=expr :: withExprAcc.usedTopLevelExprs
      )
  }

  def apply(exprs : List[et.Expr]) = {
    // Fold right because we want to know if a binding is used by the time we encounter it
    // This means we must visit them in reverse order
    exprs.foldRight(AnalysedExprs()) { case (expr, previousAcc) =>
      handleTopLevelExpr(expr, previousAcc)
    }
  }
}
