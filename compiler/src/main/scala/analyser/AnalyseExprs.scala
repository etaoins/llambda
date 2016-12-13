package io.llambda.compiler.analyser
import io.llambda

import llambda.compiler.{StorageLocation, et}
import llambda.compiler.InternalCompilerErrorException

case class AnalysedExprs(
  usedTopLevelExprs: List[et.Expr] = List(),
  mutableVars: Set[StorageLocation] = Set(),
  constantTopLevelBindings: List[(StorageLocation, et.Expr)] = Nil,
  varUses: Map[StorageLocation, Int] = Map(),
  nativeSymbols: Set[String] = Set(),
  parameterized: Boolean = false
)

object AnalyseExprs  {
  private def handleNestedExpr(expr: et.Expr, acc: AnalysedExprs): AnalysedExprs = expr match {
    case _: et.TopLevelDefine =>
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
      // We found a use
      acc.copy(
        varUses=acc.varUses + (storageLoc -> (acc.varUses.getOrElse(storageLoc, 0) + 1))
      )

    case et.NativeFunction(_, _, nativeSymbol) =>
      acc.copy(
        nativeSymbols=acc.nativeSymbols + nativeSymbol
      )

    case parameterize: et.Parameterize =>
      parameterize.subexprs.foldLeft(acc) { case (previousAcc, expr) =>
        handleNestedExpr(expr, previousAcc)
      }.copy(parameterized=true)

    case other =>
      // Iterate over our subexprs
      other.subexprs.foldLeft(acc) { case (previousAcc, expr) =>
        handleNestedExpr(expr, previousAcc)
      }
  }

  private def handleTopLevelExpr(expr: et.Expr, acc: AnalysedExprs): AnalysedExprs = expr match {
    case et.TopLevelDefine(binding @ et.Binding(storageLoc, initialiser)) =>
      // Filter out the bindings that are unused
      val bindingUsed = TopLevelDefineRequired(storageLoc, initialiser, acc)

      if (!bindingUsed) {
        // Nothing to do!
        // Drop us completely
        acc
      }
      else {
        // Add it to the list of used expressions
        val accWithUsedDefine = acc.copy(
          usedTopLevelExprs=expr :: acc.usedTopLevelExprs
        )

        // Deal with the initialisers
        val accWithBinding = if (accWithUsedDefine.mutableVars.contains(storageLoc)) {
          // Don't record the initialisers for mutable top-level bindings
          // They provide no actionable information on the value of the storage location
          accWithUsedDefine
        }
        else {
          // Record our initialiser
          accWithUsedDefine.copy(
            constantTopLevelBindings=(storageLoc -> initialiser) :: accWithUsedDefine.constantTopLevelBindings
          )
        }

        // Recurse down our initialiser
        handleNestedExpr(initialiser, accWithBinding)
      }

    case _ =>
      val withExprAcc = handleNestedExpr(expr, acc)

      // This is a non-(define)
      // Leave it in the used expression list
      withExprAcc.copy(
        usedTopLevelExprs=expr :: withExprAcc.usedTopLevelExprs
      )
  }

  def apply(exprs: List[et.Expr]) = {
    // Fold right because we want to know if a binding is used by the time we encounter it
    // This means we must visit them in reverse order
    exprs.foldRight(AnalysedExprs()) { case (expr, previousAcc) =>
      handleTopLevelExpr(expr, previousAcc)
    }
  }
}
