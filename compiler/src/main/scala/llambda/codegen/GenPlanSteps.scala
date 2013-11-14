package llambda.codegen

import llambda.planner.{step => ps}

object GenPlanSteps {
  def apply(initialState : GenerationState)(steps : List[ps.Step]) : GenerationState = 
    steps.foldLeft(initialState) { case (state, step) =>
      GenPlanStep(state)(step)
    }
}

