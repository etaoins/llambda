package llambda.planner

import llambda.et
import llambda.planner.{step => ps}
import llambda.analyzer.AnalysisResult

object PlanExpressions {
  def apply(exprs : List[et.Expression])(analysis : AnalysisResult) : List[ps.Step] = {
    val emptyState = PlannerState() 
    val planSteps = new StepBuffer
      
    PlanExpression(emptyState)(et.Begin(exprs))(analysis, planSteps)

    planSteps.toList
  }
}
