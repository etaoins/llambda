package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{PolymorphicSignature, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.planner.reportproc.ReportProcPlanner

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param polySignature  Polymorphic signature of the procedure
  * @param plannedSymbol  Native symbol of the direct entry point to the procedure
  * @param selfTempOpt    For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                       point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                       if this value is explicitly converted to a ct.ProcedureCell
  * @param reportNameOpt  Name of this procedure in R7RS. This is used as a tag to implement certain optimizations
  *                       elsewhere in the planner. It is not directly used by this class
  */
class KnownUserProc(
    polySignature : PolymorphicSignature,
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    val reportNameOpt : Option[String] = None
) extends KnownProc(polySignature, selfTempOpt) {
  def nativeSymbol(implicit plan : PlanWriter) : String =
    plannedSymbol

  def withReportName(newReportName : String) : KnownUserProc = {
    new KnownUserProc(polySignature, plannedSymbol, selfTempOpt, Some(newReportName))
  }

  override def withSelfTemp(selfTemp : ps.TempValue) : KnownUserProc = {
    new KnownUserProc(polySignature, plannedSymbol, Some(selfTemp), reportNameOpt)
  }

  override def attemptInlineApplication(state : PlannerState)(
      args : List[(ContextLocated, IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[PlanResult] = {
    val reportProcPlanners = ReportProcPlanner.activePlanners

    // Find the first report proc planner that knowns how to plan us
    for(reportName <- reportNameOpt;
        reportProcPlanner <- reportProcPlanners;
        planResult <- reportProcPlanner.planWithResult(state)(reportName, args)) {
      return Some(planResult)
    }

    None
  }
}

