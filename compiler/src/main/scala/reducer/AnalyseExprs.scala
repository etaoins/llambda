package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.{StorageLocation, et}
import llambda.compiler.InternalCompilerErrorException

case class AnalysedExprs(
  usedTopLevelExprs : List[et.Expr] = List(),
  mutableVars : Set[StorageLocation] = Set(),
  constantTopLevelBindings : Map[StorageLocation, et.Expr] = Map(),
  usedVars : Set[StorageLocation] = Set()
)

object AnalyseExprs  {
  private def handleNestedExpr(expr : et.Expr, acc : AnalysedExprs) : AnalysedExprs = expr match {
    case _ : et.TopLevelDefinition =>
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
    case et.TopLevelDefinition(bindings) =>
      // Filter out the bindings that are unused
      val usedBindings = bindings.filter { case (storageLoc, initialiser) =>
        // Is this value directly used?
        acc.usedVars.contains(storageLoc) ||
          // Or does it need to be initialised for a later mutation?
          acc.mutableVars.contains(storageLoc) ||
          // Or does the initialiser have side effects?
          ExprHasSideEffects(initialiser)
      }

      if (usedBindings.isEmpty) {
        // Nothing to do!
        // Drop us completely
        acc
      }
      else {
        // Create a new top-level define with just the used bindings
        val onlyUsedDefine = et.TopLevelDefinition(usedBindings).assignLocationFrom(expr)

        // Add it to the list of used expressions
        val accWithOnlyUsedDefine = acc.copy(
          usedTopLevelExprs=onlyUsedDefine :: acc.usedTopLevelExprs
        )

        // Deal with our initialisers
        usedBindings.foldLeft(accWithOnlyUsedDefine) { case (previousAcc, (storageLoc, initialiser)) =>
          val accWithBinding = if (previousAcc.mutableVars.contains(storageLoc)) {
            // Don't record the initialisers for mutable top-level bindings
            // They provide no actionable information on the value of the storage location
            previousAcc
          }
          else {
            // Record our initialiser
            previousAcc.copy(
              constantTopLevelBindings=previousAcc.constantTopLevelBindings + (storageLoc -> initialiser)
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
