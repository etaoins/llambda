package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{PolymorphicSignature, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.planner.stdlibproc.StdlibProcPlanner
import llambda.compiler.codegen.RuntimeFunctions

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param polySignature  Polymorphic signature of the procedure
  * @param plannedSymbol  Native symbol of the direct entry point to the procedure
  * @param selfTempOpt    For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                       point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                       if this value is explicitly converted to a ct.ProcedureCell
  * @param stdlibNameOpt  Name of this procedure in the standard library. This is used as a tag to implement certain
  *                       optimisations in the planner.
  */
class KnownUserProc(
    polySignature: PolymorphicSignature,
    plannedSymbol: String,
    selfTempOpt: Option[ps.TempValue],
    val stdlibNameOpt: Option[String] = None
) extends KnownProc(polySignature, selfTempOpt) {
  def nativeSymbol(implicit plan: PlanWriter): String =
    plannedSymbol

  def withStdlibName(newStdlibName: String): KnownUserProc = {
    new KnownUserProc(polySignature, plannedSymbol, selfTempOpt, Some(newStdlibName))
  }

  override def withSelfTemp(selfTemp: ps.TempValue): KnownUserProc = {
    new KnownUserProc(polySignature, plannedSymbol, Some(selfTemp), stdlibNameOpt)
  }

  override def attemptInlineApplication(state: PlannerState)(
      args: List[(ContextLocated, IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = {
    val stdlibProcPlanners = StdlibProcPlanner.activePlanners

    // Find the first stdlib proc planner that knowns how to plan us
    for(stdlibName <- stdlibNameOpt;
        stdlibProcPlanner <- stdlibProcPlanners;
        planResult <- stdlibProcPlanner.planWithResult(state)(stdlibName, args)) {
      return Some(planResult)
    }

    None
  }

  override def hasSideEffects(arity: Int): Boolean =
    RuntimeFunctions.hasSideEffects(plannedSymbol, arity)
}

