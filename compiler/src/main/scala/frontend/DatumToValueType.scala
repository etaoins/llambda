package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.PrimitiveExpressions
import llambda.compiler.{valuetype => vt}
import llambda.compiler.sst
import llambda.compiler.{BoundType, BadSpecialFormException}

object DatumToValueType {
  def apply(datum : sst.ScopedDatum) : vt.ValueType = datum match { 
    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

        case PrimitiveExpressions.WorldPointer =>
          throw new BadSpecialFormException(symbol, "world-pointer can only be used in the initial position of a (native-function) argument list")

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }

    case nonsymbol => 
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol")
  }
}
