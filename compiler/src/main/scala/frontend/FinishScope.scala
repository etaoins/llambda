package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

object FinishScope {
  def apply(scope: Scope) = {
    for((declaredSymbol, _) <- scope.typeDeclarations) {
      scope.bindings.get(declaredSymbol.name) match {
        case Some(_: StorageLocation) =>
          // This is okay

        case _ =>
          throw new UnboundVariableException(declaredSymbol, "Declared symbol has no variable definition")
      }
    }
  }
}
