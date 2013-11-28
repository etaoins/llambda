package llambda.frontend

import llambda.{valuetype => vt}
import llambda.sst
import llambda.{BoundType, BadSpecialFormException}

object DatumToValueType {
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
