package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.{StorageLocation, et}

private[analyzer] case class FoundVars(
  mutableVars : Set[StorageLocation] = Set(),
  topLevelBindings : Map[StorageLocation, et.Expression] = Map(),
  usedVars : Set[StorageLocation] = Set()
) {
  def ++(other : FoundVars) : FoundVars =
    FoundVars(
      mutableVars=mutableVars ++ other.mutableVars,
      topLevelBindings=topLevelBindings ++ other.topLevelBindings,
      usedVars=usedVars ++ other.usedVars
    )
}

private[analyzer] object FindVars {
  def apply(expr : et.Expression) : FoundVars = {
    // Find any mutable vars from our subexpressions
    val subexprFoundVars = expr.subexpressions.foldLeft(FoundVars()) { (previousFoundVars, expr) =>
      apply(expr) ++ previousFoundVars
    }

    expr match {
      case et.MutateVar(storageLoc, _) =>
        subexprFoundVars.copy(
          mutableVars=subexprFoundVars.mutableVars + storageLoc
        )

      case et.TopLevelDefinition(bindings) =>
        subexprFoundVars.copy(
          topLevelBindings=subexprFoundVars.topLevelBindings ++ bindings
        )

      case et.VarRef(storageLoc) =>
        subexprFoundVars.copy(
          usedVars=subexprFoundVars.usedVars + storageLoc
        )

      case _ =>
        subexprFoundVars
    }
  }
}

