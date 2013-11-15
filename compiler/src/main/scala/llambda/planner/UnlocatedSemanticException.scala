package llambda.planner

import llambda._

/** Planner internal class for SemanticExceptions without a source location
  *
  * LocateExceptionsWith will intercept these and rethrow a
  * LocatedSemanticException with the appropriate source location
  */
private[planner] sealed abstract class UnlocatedSemanticException(message : String) extends SemanticException(message) {
  /** Returns the LocatedSemanticException version of this exception
    * 
    * Cause does not have to be initialized here
    */
  protected def rawLocatedException(located : SourceLocated) : LocatedSemanticException

  /** Returns the LocatedSemanticException version of this exception
    *
    * The exception will have its cause initialized to the original exception
    *
    * @param  located  Source location to use when constructing the
    *                  LocatedSemanticException
    */
  def toLocatedException(located : SourceLocated) : LocatedSemanticException = {
    val newException = rawLocatedException(located)
    newException.initCause(this)
    newException
  }
}

private[planner] class UnlocatedImpossibleTypeConversionException(message : String) extends UnlocatedSemanticException(message) with ImpossibleTypeConversionExceptionLike {
  def rawLocatedException(located : SourceLocated) : LocatedSemanticException = 
    new ImpossibleTypeConversionException(located, message)
}

private[planner] class UnlocatedIncompatibleArityException(message : String) extends UnlocatedSemanticException(message) with IncompatibleArityExceptionLike {
  def rawLocatedException(located : SourceLocated) : LocatedSemanticException = 
    new IncompatibleArityException(located, message)
}
