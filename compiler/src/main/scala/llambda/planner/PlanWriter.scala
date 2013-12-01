package llambda.planner

import collection.mutable

import llambda.planner.{step => ps}
import llambda.InternalCompilerErrorException

class PlanWriter(val steps : mutable.ListBuffer[ps.Step], val plannedFunctions : mutable.Map[String, PlannedFunction]) {
  /** Allocates a unique name for a procedure
    *
    * codegen expects a flat namespace for procedures but the same name can
    * appear in multiple Scheme scopes 
    */
  def allocProcedureSymbol(sourceName : String) : String = {
    if (!plannedFunctions.contains(sourceName)) {
      sourceName
    }
    else {
      for(suffix <- Stream.from(1)) {
        val suffixedName = sourceName + "_" + suffix.toString

        if (!plannedFunctions.contains(suffixedName)) {
          return suffixedName
        }
      }

      throw new InternalCompilerErrorException("Ran out of natural numbers")
    }
  }

  def forkPlan() : PlanWriter = 
    // All forks share plannedFunctions
    new PlanWriter(new mutable.ListBuffer[ps.Step], plannedFunctions)

  def buildCondBranch(test : ps.TempValue, trueBuilder : (PlanWriter) => ps.TempValue, falseBuilder : (PlanWriter) => ps.TempValue) : ps.TempValue = {
    val phiTemp = new ps.TempValue

    val truePlan = forkPlan()
    val trueValue = trueBuilder(truePlan) 

    val falsePlan = forkPlan()
    val falseValue = falseBuilder(falsePlan)

    this.steps += ps.CondBranch(phiTemp, test, truePlan.steps.toList, trueValue, falsePlan.steps.toList, falseValue)

    // Return the phi'ed value
    phiTemp
  }
}

object PlanWriter {
  def apply() =
    new PlanWriter(new mutable.ListBuffer[ps.Step], new mutable.HashMap[String, PlannedFunction])
}
