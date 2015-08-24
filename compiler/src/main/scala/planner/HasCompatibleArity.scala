package io.llambda.compiler.planner
import io.llambda

private[planner] object HasCompatibleArity {
  /**
   * Determines if the number of supplied arguments satisfies a procedure's arity
   *
   * @param  suppliedArgs   Number of arguments supplied to the procedure
   * @param  mandatoryArgs  Number of mandatory arguments the procedure requires
   * @param  optionalArg    Number of optional arguments the procedure accepts
   * @param  hasRestARg     Indicates if the procedures accepts rest arguments
   */
  def apply(suppliedArgs : Int, mandatoryArgs : Int, optionalArgs : Int, hasRestArg : Boolean) : Boolean =
    (suppliedArgs >= mandatoryArgs) && ((suppliedArgs <= (mandatoryArgs + optionalArgs)) || hasRestArg)
}
