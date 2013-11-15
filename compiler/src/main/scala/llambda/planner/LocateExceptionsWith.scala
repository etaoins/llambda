package llambda.planner

import llambda.SourceLocated
import scala.util.parsing.input.NoPosition

/** Catches any UnlocatedSemanticExceptions and rethrows them with a location
  *
  * This allows us to wrap code working with a single Expression in a large
  * block without having to pass the Expression to every method that may throw
  * a SemanticException
  */
object LocateExceptionsWith {
  def apply[T](sourceLocated : SourceLocated)(block : => T) = {
    if (sourceLocated.pos eq NoPosition) {
      // We don't have a position, just pass through
      block
    }
    else {
      try {
        block
      }
      catch {
        case unlocatedException : UnlocatedSemanticException =>
          println("RELOCATING")
          throw unlocatedException.toLocatedException(sourceLocated)
      }
    }
  }
}

