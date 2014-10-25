package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param signature      Signature of the procedure
  * @param plannedSymbol  Native symbol of the direct entry point to the procedure
  * @param selfTempOpt    For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                       point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                       if this value is explicitly converted to a ct.ProcedureCell
  * @param reportNameOpt  Name of this procedure in R7RS. This is used as a tag to implement certain optimizations
  *                       elsewhere in the planner. It is not directly used by this class
  */
class KnownUserProc(
    signature : ProcedureSignature,
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    val reportNameOpt : Option[String] = None
) extends KnownProc(signature, selfTempOpt) {
  //
  // These objects know how to implement certain report procedure directly with plan steps
  // 
  
  private val correctnessPlanners = List[reportproc.ReportProcPlanner](
    // These are needed for occurrence typing
    reportproc.EquivalenceProcPlanner,
    reportproc.NumberPredicateProcPlanner,
    // Planning (values) allows proper type signatures to be extracted
    reportproc.ValuesProcPlanner
  )
  
  private val optimisingPlanners = List[reportproc.ReportProcPlanner](
    reportproc.ApplyProcPlanner,
    reportproc.ArithmeticProcPlanner,
    reportproc.BytevectorProcPlanner,
    reportproc.CadrProcPlanner,
    reportproc.CharProcPlanner,
    reportproc.DynamicProcPlanner,
    reportproc.ListProcPlanner,
    reportproc.NumberProcPlanner,
    reportproc.StringProcPlanner,
    reportproc.VectorProcPlanner
  )

  def nativeSymbol(implicit plan : PlanWriter) : String =
    plannedSymbol

  def withReportName(newReportName : String) : KnownUserProc = {
    new KnownUserProc(signature, plannedSymbol, selfTempOpt, Some(newReportName))
  }
  
  override def withSelfTemp(selfTemp : ps.TempValue) : KnownUserProc = {
    new KnownUserProc(signature, plannedSymbol, Some(selfTemp), reportNameOpt)
  }
  
  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    val reportProcPlanners = if (plan.config.optimize) {
      correctnessPlanners ++ optimisingPlanners
    }
    else {
      correctnessPlanners
    }

    // Find the first report proc planner that knowns how to plan us
    for(reportName <- reportNameOpt;
        reportProcPlanner <- reportProcPlanners;
        planResult <- reportProcPlanner.planWithResult(state)(reportName, operands)) {
      return Some(planResult)
    }

    None
  }
}

