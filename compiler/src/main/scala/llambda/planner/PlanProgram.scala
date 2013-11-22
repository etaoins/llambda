package llambda.planner

import llambda.et
import llambda.planner.{step => ps}
import llambda.analyzer.AnalysisResult
import llambda.codegen.LlambdaExecSignature
import llambda.nfi

object PlanProgram {
  def apply(exprs : List[et.Expression])(analysis : AnalysisResult) : Map[String, PlannedFunction] = {
    val emptyState = PlannerState() 
    val plan = PlanWriter()
      
    PlanExpression(emptyState)(et.Begin(exprs))(analysis, plan)

    // __llambda_exec is a void function
    plan.steps += ps.Return(None)

    (plan.plannedFunctions + (LlambdaExecSignature.nativeSymbol -> PlannedFunction(
      signature=LlambdaExecSignature,
      steps = plan.steps.toList 
    ))).toMap
  }
}
