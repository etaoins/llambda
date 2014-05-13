package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

object ReferencedVariables {
  /** Finds all variables used by an expression and its subexpressions */ 
  def apply(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(storageLoc) => 
      Set(storageLoc)

    case other =>
      other.subexpressions.flatMap(ReferencedVariables(_)).toSet
  }

}
