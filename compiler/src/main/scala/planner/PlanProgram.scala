package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, NativeLibrary}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.codegen.LlambdaTopLevelSignature

case class PlannedProgram(
    functions : Map[String, PlannedFunction],
    requiredNativeLibraries : Set[NativeLibrary]
)

object PlanProgram {
  def apply(exprs : List[et.Expr])(planConfig : PlanConfig) : PlannedProgram = {
    val worldTemp = new ps.WorldPtrValue

    val emptyState = PlannerState(
      worldPtr=worldTemp,
      inlineDepth=0
    )

    val plan = PlanWriter(planConfig)
      
    PlanExpr(emptyState)(et.Begin(exprs))(plan)

    // __llambda_top_level is a void function
    plan.steps += ps.Return(None)

    val allPlannedFunctions = 
      (plan.plannedFunctions + (LlambdaTopLevelSignature.nativeSymbol -> PlannedFunction(
        signature=LlambdaTopLevelSignature,
        namedArguments=List("world" -> worldTemp),
        steps=plan.steps.toList,
        worldPtrOpt=Some(worldTemp),
        debugContextOpt=None
      ))).toMap

    val usedFunctions = FindUsedFunctions(allPlannedFunctions, LlambdaTopLevelSignature.nativeSymbol)

    PlannedProgram(usedFunctions, plan.requiredNativeLibraries.toSet)
  }
}
