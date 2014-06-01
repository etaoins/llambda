package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

object ReferencedVariables {
  /** Finds all variables used by an expression and its subexprs */ 
  def apply(expr : et.Expr) : Set[StorageLocation] = expr match {
    case et.VarRef(storageLoc) => 
      Set(storageLoc)

    case other =>
      other.subexprs.flatMap(ReferencedVariables(_)).toSet
  }

}
