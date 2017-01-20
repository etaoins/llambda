package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{ContextLocated, NoContextLocation}
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.{ProcedureSignature, NativeLibrary}
import llambda.compiler.{valuetype => vt}

class PlanWriter(
    val config: PlanConfig,
    val plannedFunctions: mutable.Map[String, PlannedFunction],
    val allocedSymbols: mutable.HashSet[String],
    val plannedTypePredicates: mutable.Map[vt.SchemeType, String],
    val knownProcTrampolines: mutable.Map[(String, ProcedureSignature), String],
    val adapterProcTrampolines: mutable.Map[(ProcedureSignature, ProcedureSignature), String],
    val polymorphInstances: mutable.Map[(LambdaManifest, vt.ProcedureType), String],
    val requiredNativeLibraries: mutable.HashSet[NativeLibrary]
) {
  private var contextLocStack: List[ContextLocated] = Nil

  private var planSealed = false

  class StepBuilder {
    private val stepBuffer = new mutable.ListBuffer[ps.Step]

    def +=(step: ps.Step) {
      if (planSealed) {
        throw new InternalCompilerErrorException("Attempt to write to sealed plan")
      }

      for(contextLoc <- contextLocStack.headOption) {
        // Context locate this step
        step.assignLocationFrom(contextLoc)
      }

      stepBuffer += step
    }

    def ++=(other: PlanWriter#StepBuilder) = {
      if (planSealed) {
        throw new InternalCompilerErrorException("Attempt to write to sealed plan")
      }

      // Assume these steps were properly located when the other plan was built
      stepBuffer ++= other.stepBuffer
    }

    def toList: List[ps.Step] =
      stepBuffer.toList
  }

  val steps = new StepBuilder

  /** Pushes a new source location on to the location stack for the duration of block
    *
    * This will implicitly locate any plan steps added while the block is being executed
    */
  def withContextLocation[T](contextLoc: ContextLocated)(block: => T): T = {
    contextLocStack = contextLoc :: contextLocStack

    try {
      block
    }
    finally {
      contextLocStack = contextLocStack.tail
    }
  }

  /** Returns the active source location or NoContextLocation if the location is not available */
  def activeContextLocated =
    contextLocStack.headOption.getOrElse(NoContextLocation)

  def withContextLocationOpt[T](contextLocatedOpt: Option[ContextLocated])(block: => T): T = {
    contextLocatedOpt match {
      case Some(contextLocated) =>
        withContextLocation(contextLocated)(block)

      case _ =>
        block
    }
  }

  private def findNextFreeSymbol(sourceName: String): String = {
    val allKnownSymbols = plannedFunctions.keySet ++ allocedSymbols

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

  /** Allocates a unique name for a procedure or function
    *
    * codegen expects a flat namespace for procedures but the same name can appear in multiple Scheme scopes
    */
  def allocSymbol(sourceName: String): String = {
    val freeSymbol = findNextFreeSymbol(sourceName)
    allocedSymbols += freeSymbol

    freeSymbol
  }

  def forkPlan(): PlanWriter = {
    // All forks share their state except for steps
    val forkedPlan = new PlanWriter(
      config,
      plannedFunctions,
      allocedSymbols,
      plannedTypePredicates,
      knownProcTrampolines,
      adapterProcTrampolines,
      polymorphInstances,
      requiredNativeLibraries
    )

    forkedPlan.contextLocStack = contextLocStack.take(1)

    forkedPlan
  }

  def buildCondBranch(test: ps.TempValue, trueBuilder: (PlanWriter) => ps.TempValue, falseBuilder: (PlanWriter) => ps.TempValue): ps.TempValue = {
    // Seal ourselves to catch accidental writes to the parent branch
    this.planSealed = true

    val truePlan = forkPlan()
    val trueValue = trueBuilder(truePlan)

    val falsePlan = forkPlan()
    val falseValue = falseBuilder(falsePlan)

    val resultGcManaged = trueValue.isGcManaged || falseValue.isGcManaged
    val phiTemp = new ps.TempValue(resultGcManaged)

    val valuePhi = ps.ValuePhi(phiTemp, trueValue, falseValue)

    this.planSealed = false
    this.steps += ps.CondBranch(test, truePlan.steps.toList, falsePlan.steps.toList, List(valuePhi))

    // Return the phi'ed value
    phiTemp
  }
}

object PlanWriter {
  def apply(planConfig: PlanConfig) =
    new PlanWriter(
      config=planConfig,
      plannedFunctions=new mutable.HashMap[String, PlannedFunction],
      allocedSymbols=mutable.HashSet(planConfig.analysis.nativeSymbols.toSeq: _*),
      plannedTypePredicates=new mutable.HashMap[vt.SchemeType, String],
      knownProcTrampolines=new mutable.HashMap[(String, ProcedureSignature), String],
      adapterProcTrampolines=new mutable.HashMap[(ProcedureSignature, ProcedureSignature), String],
      polymorphInstances=new mutable.HashMap[(LambdaManifest, vt.ProcedureType), String],
      requiredNativeLibraries=new mutable.HashSet[NativeLibrary]
    )
}
