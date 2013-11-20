package llambda.planner

import llambda.et
import llambda.planner.{step => ps}
import llambda.analyzer.AnalysisResult
import llambda.codegen.LlambdaExecSignature
import llambda.nfi

object PlanProgram {
  def apply(exprs : List[et.Expression])(analysis : AnalysisResult) : Map[String, PlannedFunction] = {
    val emptyState = PlannerState() 
    val planSteps = new StepBuffer
      
    PlanExpression(emptyState)(et.Begin(exprs))(analysis, planSteps)

    // __llambda_exec is a void function
    planSteps += ps.Return(None)

    Map(LlambdaExecSignature.nativeSymbol -> PlannedFunction(
      signature=LlambdaExecSignature,
      steps = planSteps.toList 
    ))
  }
}
