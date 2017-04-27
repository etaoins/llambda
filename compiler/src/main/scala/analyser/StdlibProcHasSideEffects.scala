package io.llambda.compiler.analyser


object StdlibProcHasSideEffects {
  // Be *very* careful here - the procedure cannot throw an exception if passed the indicated number of arguments
  // This precludes any functions that have typed parameters
  def apply(reportProcName: String, arity: Int): Boolean = (reportProcName, arity) match {
    case ("eqv?", 2) | ("equals?", 2) =>
      false

    case ("number?", 1) | ("integer?", 1) | ("flonum?", 1) | ("rational?", 1) =>
      false

    case ("boolean?", 1) | ("not?", 1) =>
      false

    case ("pair?", 1) | ("null?", 1) | ("list?", 1) | ("list", _) | ("cons", 2) =>
      false

    case ("symbol?", 1) =>
      false

    case ("char?", 1) =>
      false

    case ("vector?", 1) | ("vector", _) =>
      false

    case ("bytevector?", 1) =>
      false

    case ("string?", 1) =>
      false

    case ("procedure?", 1) =>
      false

    case ("make-parameter", 1) =>
      false

    case ("port?", 1) | ("input-port?", 1) | ("output-port?", 1) =>
      false

    case _ =>
      true
  }
}
