package llambda.planner

import collection.mutable

import llambda.planner.{step => ps}

class PlanWriter(val steps : mutable.ListBuffer[ps.Step], val plannedFunctions : mutable.Map[String, PlannedFunction]) {
  def forkPlan() : PlanWriter = 
    // All forks share plannedFunctions
    new PlanWriter(new mutable.ListBuffer[ps.Step], plannedFunctions)
}

object PlanWriter {
  def apply() =
    new PlanWriter(new mutable.ListBuffer[ps.Step], new mutable.HashMap[String, PlannedFunction])
}
