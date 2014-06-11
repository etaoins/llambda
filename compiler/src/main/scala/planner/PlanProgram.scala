package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.planner.{step => ps}
import llambda.compiler.codegen.LlambdaTopLevelSignature

object PlanProgram {
  def apply(exprs : List[et.Expr])(planConfig : PlanConfig) : Map[String, PlannedFunction] = {
    val worldTemp = new ps.WorldPtrValue

    val emptyState = PlannerState(
      worldPtr=worldTemp
    ) 

    val plan = PlanWriter()
      
    PlanExpr(emptyState)(et.Begin(exprs))(planConfig, plan)

    // __llambda_top_level is a void function
    plan.steps += ps.Return(None)

    (plan.plannedFunctions + (LlambdaTopLevelSignature.nativeSymbol -> PlannedFunction(
      signature=LlambdaTopLevelSignature,
      namedArguments=List("world" -> worldTemp),
      steps=plan.steps.toList,
      worldPtrOpt=Some(worldTemp),
      sourceNameOpt=None,
      isArtificial=true
    ))).toMap
  }
}
