package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.planner.{step => ps}
import llambda.compiler.codegen.LlambdaExecSignature

object PlanProgram {
  def apply(exprs : List[et.Expr])(planConfig : PlanConfig) : Map[String, PlannedFunction] = {
    val worldTemp = new ps.WorldPtrValue

    val emptyState = PlannerState(
      worldPtr=worldTemp
    ) 

    val plan = PlanWriter()
      
    PlanExpr(emptyState)(et.Begin(exprs))(planConfig, plan)

    // __llambda_exec is a void function
    plan.steps += ps.Return(None)

    (plan.plannedFunctions + (LlambdaExecSignature.nativeSymbol -> PlannedFunction(
      signature=LlambdaExecSignature,
      namedArguments=List("world" -> worldTemp),
      steps=plan.steps.toList,
      worldPtrOption=Some(worldTemp),
      sourceNameOption=None,
      isArtificial=true
    ))).toMap
  }
}
