package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}

object GenPlanSteps {
  def apply(initialState : GenerationState, plannedSymbols : Set[String], typeGenerator : TypeGenerator)(steps : List[ps.Step]) : GenResult = 
    steps match {
      case step :: stepsTail =>
        GenPlanStep(initialState, plannedSymbols, typeGenerator)(step) match {
          case newState : GenerationState =>
            apply(newState, plannedSymbols, typeGenerator)(stepsTail)

          case terminated : BlockTerminated =>
            // We've terminated - don't go bother with the rest of the steps
            terminated
        }

      case Nil => 
        initialState
    }
}

