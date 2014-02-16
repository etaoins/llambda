package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.planner.{step => ps}
import llambda.compiler.analyzer.AnalysisResult
import llambda.compiler.codegen.LlambdaExecSignature

object PlanProgram {
  def apply(exprs : List[et.Expression])(planConfig : PlanConfig) : Map[String, PlannedFunction] = {
    val emptyState = PlannerState() 
    val plan = PlanWriter()
      
    PlanExpression(emptyState)(et.Begin(exprs))(planConfig, plan)

    val worldTemp = ps.GcUnmanagedValue()

    // __llambda_exec is a void function
    plan.steps += ps.Return(None)

    (plan.plannedFunctions + (LlambdaExecSignature.nativeSymbol -> PlannedFunction(
      signature=LlambdaExecSignature,
      namedArguments=List("world" -> worldTemp),
      steps = plan.steps.toList 
    ))).toMap
  }
}
