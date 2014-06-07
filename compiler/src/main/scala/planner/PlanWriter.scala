package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{SourceLocated, NoSourceLocation}
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.et

class PlanWriter(val plannedFunctions : mutable.Map[String, PlannedFunction], val allocedProcSymbols : mutable.HashSet[String]) {
  private val sourceLocStack = new mutable.Stack[SourceLocated] 

  class StepBuilder {
    private val stepBuffer = new mutable.ListBuffer[ps.Step] 

    def +=(step : ps.Step) {
      for(sourceLoc <- sourceLocStack.headOption) {
        // Source locate this step
        step.assignLocationFrom(sourceLoc)
      }

      stepBuffer += step
    }

    def toList : List[ps.Step] = 
      stepBuffer.toList
  }

  val steps = new StepBuilder

  /** Pushes a new source location on to the location stack for the duration of block
    *
    * This will implicitly locate any plan steps added while the block is being executed
    */
  def withSourceLocation[T](sourceLocated : SourceLocated)(block : => T) : T = {
    sourceLocStack.push(sourceLocated)

    try {
      block
    }
    finally {
      sourceLocStack.pop
    }
  }

  /** Returns the active source location or NoSourceLocation if the location is not available */
  def activeSourceLocated =
    sourceLocStack.headOption.getOrElse(NoSourceLocation)

  def withSourceLocationOpt[T](sourceLocatedOpt : Option[SourceLocated])(block : => T) : T = {
    sourceLocatedOpt match {
      case Some(sourceLocated) =>
        withSourceLocation(sourceLocated)(block)

      case _ =>
        block
    }
  }

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
    new PlanWriter(plannedFunctions, allocedProcSymbols)

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
    new PlanWriter(new mutable.HashMap[String, PlannedFunction], new mutable.HashSet[String])
}
