package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.{StorageLocation, et}
import llambda.compiler.InternalCompilerErrorException

case class AnalysedExpressions(
  usedTopLevelExpressions : List[et.Expression] = List(),
  mutableVars : Set[StorageLocation] = Set(),
  constantTopLevelBindings : Map[StorageLocation, et.Expression] = Map(),
  usedVars : Set[StorageLocation] = Set()
)

object AnalyseExpressions  {
  private def handleNestedExpression(expr : et.Expression, acc : AnalysedExpressions) : AnalysedExpressions = expr match {
    case _ : et.TopLevelDefinition =>
      // This voids our warranty
      throw new InternalCompilerErrorException("Top-level definition found nested in other expression")

    case et.MutateVar(storageLoc, valueExpr) =>
      // Recurse down our initialiser
      val withValueAcc = handleNestedExpression(valueExpr, acc)

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
      // Iterate over our subexpressions
      other.subexpressions.foldLeft(acc) { case (previousAcc, expr) =>
        handleNestedExpression(expr, previousAcc)
      }
  }

  private def handleTopLevelExpression(expr : et.Expression, acc : AnalysedExpressions) : AnalysedExpressions = expr match {
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
          usedTopLevelExpressions=onlyUsedDefine :: acc.usedTopLevelExpressions
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
          handleNestedExpression(initialiser, accWithBinding)
        }
      }

    case _ =>
      val withExprAcc = handleNestedExpression(expr, acc)

      // This is a non-(define)
      // Leave it in the used expression list
      withExprAcc.copy(
        usedTopLevelExpressions=expr :: withExprAcc.usedTopLevelExpressions
      )
  }

  def apply(expressions : List[et.Expression]) = {
    // Fold right because we want to know if a binding is used by the time we encounter it
    // This means we must visit them in reverse order
    expressions.foldRight(AnalysedExpressions()) { case (expr, previousAcc) =>
      handleTopLevelExpression(expr, previousAcc)
    }
  }
}
