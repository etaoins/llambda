package io.llambda.compiler.interpreter
import io.llambda

import llambda.compiler.ast
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

sealed abstract trait LiveValue
case class DatumCell(value: ast.Datum, cellType: ct.CellType) extends LiveValue
case class RuntimeEntryPoint(function: RuntimeFunction) extends LiveValue

case class InterpreterState(
    liveTemps: Map[ps.TempValue, LiveValue],
    stdout: String,
    stderr: String
)

object InterpreterState {
  val Empty = InterpreterState(
    liveTemps=Map(),
    stdout="",
    stderr=""
  )
}
