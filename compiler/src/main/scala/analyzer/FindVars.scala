package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.{StorageLocation, et}

private[analyzer] case class FoundVars(
  mutableVars : Set[StorageLocation] = Set(),
  initializers : Map[StorageLocation, et.Expression] = Map()
) {
  def ++(other : FoundVars) : FoundVars =
    FoundVars(
      mutableVars=mutableVars ++ other.mutableVars,
      initializers=initializers ++ other.initializers
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

      case et.Bind(bindings) =>
        subexprFoundVars.copy(
          initializers=subexprFoundVars.initializers ++ bindings
        )

      case _ =>
        subexprFoundVars
    }
  }
}

