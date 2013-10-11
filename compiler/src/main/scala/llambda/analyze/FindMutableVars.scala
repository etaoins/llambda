package llambda.analyze

import llambda.{StorageLocation, et}

object FindMutableVars {
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

