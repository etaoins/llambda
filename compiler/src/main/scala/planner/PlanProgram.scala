package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, NativeLibrary}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.codegen.LlambdaTopLevelSignature

case class PlannedProgram(
    functions: Map[String, PlannedFunction],
    requiredNativeLibraries: Set[NativeLibrary]
)

object PlanProgram {
  private val conniverPasses = List[conniver.Conniver](
    conniver.MergeIdenticalSteps,
    conniver.FindTailCalls,
    // This must happen after FindTailCalls for correctneess
    // Tail calls take over the stack of the calling function and cannot take stack allocated values. FindTailCalls is
    // not aware of this while AnalyseEscapes is.
    conniver.AnalyseEscapes
  )

  def apply(exprs: List[et.Expr])(planConfig: PlanConfig): PlannedProgram = {
    val plan = PlanWriter(planConfig)

    val simplifiedExprs = if (planConfig.optimise) {
      SimplifyDynamicStates(exprs)
    }
    else {
      exprs
    }

    PlanExpr(PlannerState())(et.Begin(simplifiedExprs))(plan)

    // __llambda_top_level is a void function
    plan.steps += ps.Return(None)

    val allPlannedFunctions =
      (plan.plannedFunctions + (LlambdaTopLevelSignature.nativeSymbol -> PlannedFunction(
        signature=LlambdaTopLevelSignature,
        namedArguments=List("world" -> ps.WorldPtrValue),
        steps=plan.steps.toList,
        debugContextOpt=None
      ))).toMap

    val initialUsedFunctions = FindUsedFunctions(allPlannedFunctions, LlambdaTopLevelSignature.nativeSymbol)

    val optimisedFunctions = if (planConfig.optimise) {
      conniverPasses.foldLeft(initialUsedFunctions) { case (functions, conniverPass) =>
        conniverPass(functions)
      }
    }
    else {
      // This is required for correctness
      conniver.FindTailCalls(initialUsedFunctions)
    }

    // Discard any unused steps
    val discardedFunctions = optimisedFunctions.mapValues(DiscardSteps(_))

    // Prune unused functions again
    val finalUsedFunctions = FindUsedFunctions(discardedFunctions, LlambdaTopLevelSignature.nativeSymbol)

    // Plan our cell allocations after all optimisations have been done
    val allocatedFunctions = finalUsedFunctions.mapValues(PlanHeapAllocations(_))

    PlannedProgram(allocatedFunctions, plan.requiredNativeLibraries.toSet)
  }
}
