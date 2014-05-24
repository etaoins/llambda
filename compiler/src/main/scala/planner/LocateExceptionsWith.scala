package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.SourceLocated

/** Catches any UnlocatedSemanticExceptions and rethrows them with a location
  *
  * This allows us to wrap code working with a single Expression in a large
  * block without having to pass the Expression to every method that may throw
  * a SemanticException
  */
object LocateExceptionsWith {
  def apply[T](sourceLocated : SourceLocated)(block : => T) = {
    if (!sourceLocated.locationOpt.isDefined) {
      // We don't have a position, just pass through
      block
    }
    else {
      try {
        block
      }
      catch {
        case unlocatedException : UnlocatedSemanticException =>
          throw unlocatedException.toLocatedException(sourceLocated)
      }
    }
  }
}

