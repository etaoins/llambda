package io.llambda.compiler.interpreter
import io.llambda

import llambda.compiler.planner.PlannedProgram
import llambda.compiler.RunResult
import llambda.compiler.codegen.LlambdaTopLevelSignature

object InterpretProgram {
  /** Attempts to intepret the passed PlannedProgram and returns the result
    *
    * Only very trivial programs can be interpreted. The rest will raise an UninterpretableException
    */
  def apply(program : PlannedProgram) : RunResult = {
    val entryFunction = program.functions(LlambdaTopLevelSignature.nativeSymbol)

    val initialState = InterpreterState.Empty
    val finalState = entryFunction.steps.foldLeft(initialState) { case (state, step) =>
      InterpretStep(state, step)
    }

    RunResult(finalState.stdout, finalState.stderr, 0)
  }
}
