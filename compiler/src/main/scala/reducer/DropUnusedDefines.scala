package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler._

object DropUnusedDefines {
  def apply(expr : et.Expression, usedStorageLocs : Set[StorageLocation]) : Option[et.Expression] = expr match {
    case et.TopLevelDefinition(bindings) =>
      val usedBindings = bindings.filter { case (storageLoc, initializer) =>
        usedStorageLocs.contains(storageLoc) || ExprHasSideEffects(initializer)
      }

      usedBindings match {
        case Nil =>
          None

        case nonEmpty =>
          Some(
            et.TopLevelDefinition(usedBindings).assignLocationFrom(expr)
          )
      }


    case otherExpr =>
      Some(otherExpr)
  }
}
