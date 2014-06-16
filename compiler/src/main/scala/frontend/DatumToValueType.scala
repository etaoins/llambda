package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.sst
import llambda.compiler.{BoundType, BadSpecialFormException}

object DatumToValueType {
  def toSchemeType(datum : sst.ScopedDatum) : vt.CellValueType = apply(datum) match {
    case cellValue : vt.CellValueType =>
      cellValue

    case nonCellValue =>
      throw new BadSpecialFormException(datum, "Scheme values cannot be annotated with native types")
  }

  def apply(datum : sst.ScopedDatum) : vt.ValueType = datum match { 
    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }

    case nonsymbol => 
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol")
  }
}
