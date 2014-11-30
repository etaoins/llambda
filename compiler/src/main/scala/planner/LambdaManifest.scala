package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.StorageLocation
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Manifest for a planned lambda
  *
  * This contains the information required to re-plan the lambda to either create new polymorphs or for inlining. In
  * particular, it contains a complete description of the lambda's closure which can be used with [[LoadClosureData]].
  *
  * @param  parentState          Planner state of the lambda's environment
  * @param  closureType          Record-like type of the closure used by codegen
  * @param  closedVars           List of closed variables as returned by [[FindClosedVars]]
  * @param  lambdaExpr           Original planned lambda expression
  * @param  recursiveSelfLocOpt  For recursive procedures the storage location refering to the "self" value
  */
private[planner] case class LambdaManifest(
    parentState : PlannerState,
    closureType : vt.ClosureType,
    closedVars : List[ClosedVariable],
    lambdaExpr : et.Lambda,
    recursiveSelfLocOpt : Option[StorageLocation]
) {
  /** Returns if this lambda is recursive */
  def isRecursive =
    recursiveSelfLocOpt.isDefined

  /** Returns a list of all captured variables in the lambda's closure */
  lazy val capturedVars : List[CapturedVariable] =
    closedVars collect {
      case captured : CapturedVariable => captured
    }
}
