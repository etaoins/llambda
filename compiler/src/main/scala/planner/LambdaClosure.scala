package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}

/** Describes the structure of a planner lambda's closure
  *
  * @param  closureType        Record-like type for the closure. This is required for codegen to read values out of the
  *                            procedure value
  * @param  capturedVariables  List of captured variables contained in the closure
  */
private[planner] case class LambdaClosure(
    closureType : vt.ClosureType,
    capturedVariables : List[CapturedVariable]
)
