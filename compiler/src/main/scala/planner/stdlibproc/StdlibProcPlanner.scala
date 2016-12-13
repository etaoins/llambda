package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Optionally replaces a call to a stdlib procedure with plan steps */
abstract trait StdlibProcPlanner {
  def planFromExprs(initialState: PlannerState)(
      reportName: String,
      args: List[et.Expr]
  )(implicit plan: PlanWriter): Option[PlanResult] =
    None

  def planWithResult(initialState: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = {
    planWithValue(initialState)(reportName, args) map { value =>
      PlanResult(
        state=initialState,
        value=value
      )
    }
  }

  def planWithValue(initialState: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] =
    None
}

object StdlibProcPlanner {
  // These planners enhance type information avaliable to the compiler in addition to optimising
  val typingPlanners = List[StdlibProcPlanner](
    ApplyProcPlanner,
    EquivalenceProcPlanner
  )

  // These planners primarily exist for optimisation
  val optimisingPlanners = List[StdlibProcPlanner](
    ArithmeticProcPlanner,
    BooleanProcPlanner,
    BytevectorProcPlanner,
    CadrProcPlanner,
    CharProcPlanner,
    DynamicProcPlanner,
    ListProcPlanner,
    NumberProcPlanner,
    StringProcPlanner,
    SymbolProcPlanner,
    VectorProcPlanner
  )

  def activePlanners(implicit plan: PlanWriter) =
    if (plan.config.optimise) {
      typingPlanners ++ optimisingPlanners
    }
    else {
      typingPlanners
    }
}
