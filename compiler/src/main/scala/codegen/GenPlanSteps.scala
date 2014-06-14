package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}

object GenPlanSteps {
  def apply(initialState : GenerationState, genGlobals : GenGlobals)(steps : List[ps.Step]) : GenResult = 
    steps match {
      case step :: stepsTail =>
        GenPlanStep(initialState, genGlobals)(step) match {
          case newState : GenerationState =>
            apply(newState, genGlobals)(stepsTail)

          case terminated : BlockTerminated =>
            // We've terminated - don't go bother with the rest of the steps
            terminated
        }

      case Nil => 
        initialState
    }
}

