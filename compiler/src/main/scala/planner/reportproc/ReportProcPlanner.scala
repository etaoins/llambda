package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Optionally replaces a call to a report procedure with plan steps */
abstract trait ReportProcPlanner {
  def planFromExprs(initialState : PlannerState)(
      reportName : String,
      args : List[et.Expr]
  )(implicit plan : PlanWriter) : Option[PlanResult] =
    None

  def planWithResult(initialState : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[PlanResult] = {
    planWithValues(initialState)(reportName, args) map { values =>
      PlanResult(
        state=initialState,
        values=values
      )
    }
  }

  def planWithValues(initialState : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = {
    planWithValue(initialState)(reportName, args) map { value =>
      SingleValue(value)
    }
  }

  def planWithValue(initialState : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] =
    None
}

object ReportProcPlanner {
  // These planners enhance type information avaliable to the compiler in addition to optimising
  val typingPlanners = List[ReportProcPlanner](
    ApplyProcPlanner,
    EquivalenceProcPlanner,
    NumberPredicateProcPlanner,
    ValuesProcPlanner
  )

  // These planners primarily exist for optimisation
  val optimisingPlanners = List[ReportProcPlanner](
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

  def activePlanners(implicit plan : PlanWriter) =
    if (plan.config.optimise) {
      typingPlanners ++ optimisingPlanners
    }
    else {
      typingPlanners
    }
}
