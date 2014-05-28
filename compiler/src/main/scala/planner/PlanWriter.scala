package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.compiler.InternalCompilerErrorException

class PlanWriter(val steps : mutable.ListBuffer[ps.Step], val plannedFunctions : mutable.Map[String, PlannedFunction], val allocedProcSymbols : mutable.HashSet[String]) {
  private def findNextFreeSymbol(sourceName : String) : String = {
    val allKnownSymbols = plannedFunctions.keySet ++ allocedProcSymbols

    if (!allKnownSymbols.contains(sourceName)) {
      sourceName
    }
    else {
      for(suffix <- Stream.from(1)) {
        val suffixedName = sourceName + "_" + suffix.toString

        if (!allKnownSymbols.contains(suffixedName)) {
          return suffixedName
        }
      }
      
      throw new InternalCompilerErrorException("Ran out of natural numbers")
    }
  }
  

  /** Allocates a unique name for a procedure
    *
    * codegen expects a flat namespace for procedures but the same name can appear in multiple Scheme scopes 
    */
  def allocProcedureSymbol(sourceName : String) : String = {
    val freeSymbol = findNextFreeSymbol(sourceName)
    allocedProcSymbols += freeSymbol

    freeSymbol
  }

  def forkPlan() : PlanWriter = 
    // All forks share plannedFunctions
    new PlanWriter(new mutable.ListBuffer[ps.Step], plannedFunctions, allocedProcSymbols)

  def buildCondBranch(test : ps.TempValue, trueBuilder : (PlanWriter) => ps.TempValue, falseBuilder : (PlanWriter) => ps.TempValue) : ps.TempValue = {
    val truePlan = forkPlan()
    val trueValue = trueBuilder(truePlan) 

    val falsePlan = forkPlan()
    val falseValue = falseBuilder(falsePlan)

    if (trueValue.isGcManaged != falseValue.isGcManaged) {
      throw new InternalCompilerErrorException("phi branches returning GC incompatible values")
    }

    val phiTemp = new ps.TempValue(trueValue.isGcManaged)

    this.steps += ps.CondBranch(phiTemp, test, truePlan.steps.toList, trueValue, falsePlan.steps.toList, falseValue)

    // Return the phi'ed value
    phiTemp
  }
}

object PlanWriter {
  def apply() =
    new PlanWriter(new mutable.ListBuffer[ps.Step], new mutable.HashMap[String, PlannedFunction], new mutable.HashSet[String])
}
