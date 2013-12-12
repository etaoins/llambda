package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.{StorageLocation, et}

private[analyzer] object FindMutableVars {
  def apply(expr : et.Expression) : Set[StorageLocation] = {
    // Find any mutable vars from our subexpressions
    val subexprMutableVars = expr.subexpressions.flatMap(apply).toSet

    expr match {
      case et.MutateVar(storageLoc, _) =>
        subexprMutableVars + storageLoc
      case _ =>
        subexprMutableVars
    }
  }
}

