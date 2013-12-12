package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}

object GenPlanSteps {
  def apply(initialState : GenerationState, plannedSymbols : Set[String], typeGenerator : TypeGenerator)(steps : List[ps.Step]) : GenerationState = 
    steps.foldLeft(initialState) { case (state, step) =>
      GenPlanStep(state, plannedSymbols, typeGenerator)(step)
    }
}

