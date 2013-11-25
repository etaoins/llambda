package llambda.frontend

import llambda._
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}

object ExtractNativeFunction {
  private def datumToValueType(datum : sst.ScopedDatum) : vt.ValueType = datum match { 
    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }

    case nonsymbol => 
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol")
  }

  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], returnTypeDatum : Option[sst.ScopedSymbol], nativeSymbol : String) : et.NativeFunction = {
    var fixedArgTypes = fixedArgData map datumToValueType

    val hasRestArg = restArgDatum match {
      case Some(datum) =>
        datumToValueType(datum) match {
          case vt.BoxedValue(bt.BoxedListElement) => true
          case _ =>
            throw new BadSpecialFormException(datum, "Only boxed-list-element can be used as a rest argument")
        }

      case None => false
    }

    val returnType = returnTypeDatum map datumToValueType

    et.NativeFunction(
      fixedArgs = fixedArgTypes,
      hasRestArg = hasRestArg,
      returnType = returnType,
      nativeSymbol = nativeSymbol)
  }

  def apply(operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    // These mirror the lambda forms
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedProperList(fixedArgs) :: Nil =>
      createNativeFunction(fixedArgs, None, None, nativeSymbol)

    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedProperList(fixedArgs) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(fixedArgs, None, Some(returnTypeDatum), nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: (restArgDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(Nil, Some(restArgDatum), None, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: (restArgDatum : sst.ScopedSymbol) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(Nil, Some(restArgDatum), Some(returnTypeDatum), nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedImproperList(fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: Nil =>
      createNativeFunction(fixedArgs, Some(restArgDatum), None, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedImproperList(fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(fixedArgs, Some(restArgDatum), Some(returnTypeDatum), nativeSymbol)

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native-function operands")
  }
}
