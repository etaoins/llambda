package io.llambda.compiler.interpreter
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}

object InterpretStep {
  private def tempToAstDatum(state: InterpreterState)(tempValue: ps.TempValue): ast.Datum =
    state.liveTemps.get(tempValue) match {
      case Some(DatumCell(value, _)) =>
        value

      case _ =>
        throw new UninterpretableException("Unable to reference datum")
    }

  def apply(state: InterpreterState, step: ps.Step): InterpreterState = step match {
    case ps.CreateUnitCell(resultTemp) =>
      val datumCell = DatumCell(ast.Unit(), ct.UnitCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateEmptyListCell(resultTemp) =>
      val datumCell = DatumCell(ast.EmptyList(), ct.EmptyListCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateSymbolCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.Symbol(value), ct.SymbolCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateStringCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.String(value), ct.StringCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateBooleanCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.Boolean(value), ct.BooleanCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateIntegerCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.Integer(value), ct.IntegerCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateFlonumCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.Flonum(value), ct.FlonumCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateCharCell(resultTemp, value) =>
      val datumCell = DatumCell(ast.Char(value), ct.CharCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateBytevectorCell(resultTemp, elements) =>
      val datumCell = DatumCell(ast.Bytevector(elements), ct.BytevectorCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreatePairCell(resultTemp, carTemp, cdrTemp, _) =>
      val carValue = tempToAstDatum(state)(carTemp)
      val cdrValue = tempToAstDatum(state)(cdrTemp)

      val datumCell = DatumCell(ast.Pair(carValue, cdrValue), ct.PairCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateVectorCell(resultTemp, elementTemps) =>
      val elementValues = elementTemps.map(tempToAstDatum(state))

      val datumCell = DatumCell(ast.Vector(elementValues), ct.VectorCell)
      state.copy(state.liveTemps + (resultTemp -> datumCell))

    case ps.CreateNamedEntryPoint(resultTemp, _, nativeSymbol) =>
      val runtimeFunction = RuntimeFunction.functionForSymbol.lift(nativeSymbol).getOrElse(
        throw new UninterpretableException("Unhandled runtime function: " + nativeSymbol.toString)
      )

      state.copy(state.liveTemps + (resultTemp -> RuntimeEntryPoint(runtimeFunction)))

    case ps.CastCellToTypeUnchecked(result, value, toType) =>
      val newDatumCell = state.liveTemps.get(value) match {
        case Some(datumCell: DatumCell) =>
          datumCell.copy(cellType=toType)

        case _ =>
          throw new UninterpretableException("Cast of non-cell to type")
      }

      state.copy(state.liveTemps + (result -> newDatumCell))

    case ps.TailCall(_, entryPointTemp, args) =>
      val runtimeFunction = state.liveTemps.get(entryPointTemp) match {
        case Some(RuntimeEntryPoint(runtimeFunction)) =>
          runtimeFunction

        case _ =>
          throw new UninterpretableException("Invoke of non-function")
      }

      val (newState, _) = runtimeFunction(state, args.map(state.liveTemps))
      newState

    case ps.DisposeValues(toDispose) =>
      state.copy(state.liveTemps -- toDispose)

    case other =>
      throw new UninterpretableException("Unhandled step: " + other.toString)
  }
}
