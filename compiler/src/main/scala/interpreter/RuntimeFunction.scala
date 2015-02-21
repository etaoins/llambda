package io.llambda.compiler.interpreter
import io.llambda

import llambda.compiler.ast
import llambda.compiler.{celltype => ct}

abstract class RuntimeFunction extends ((InterpreterState, List[LiveValue]) => (InterpreterState, Option[LiveValue]))

object RuntimeFunction {
  // llcore_write_stdout
  object LlcoreWriteStdout extends RuntimeFunction {
    def apply(state : InterpreterState, args : List[LiveValue]) : (InterpreterState, Option[LiveValue]) = {
      args match {
        case List(DatumCell(toWrite, ct.AnyCell)) =>
          val newState = state.copy(stdout=state.stdout + toWrite.toString)

          (newState, None)

        case _ =>
          throw new UninterpretableException("Unknown args to llcore_write_stdout")
      }
    }
  }

  val functionForSymbol : PartialFunction[String, RuntimeFunction] = {
    case "llcore_write_stdout" =>
      LlcoreWriteStdout
  }
}
