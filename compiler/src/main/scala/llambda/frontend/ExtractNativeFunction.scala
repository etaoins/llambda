package llambda.frontend

import llambda._

object ExtractNativeFunction {
  private def parseNativeType(typeDatum : sst.ScopedSymbol) : nfi.NativeType = typeDatum.name match {
    case "bool"   => nfi.CBool
    case "int8"   => nfi.Int8
    case "int16"  => nfi.Int16
    case "int32"  => nfi.Int32
    case "int64"  => nfi.Int64
    case "uint8"  => nfi.UInt8
    case "uint16" => nfi.UInt16
    case "uint32" => nfi.UInt32
    case "float"  => nfi.Float
    case "double" => nfi.Double

    case "utf8-cstring" => nfi.Utf8CString

    case "unicode-char" => nfi.UnicodeChar

    // XXX: This assumes Unix-like LP64: 64bit Linux, FreeBSD, Mac OS X, etc 
    // These aliases are here so we can do the right thing when porting to other archs
    case "short"  => nfi.Int16
    case "int"    => nfi.Int32
    case "long"   => nfi.Int64
    case "ushort" => nfi.UInt16
    case "uint"   => nfi.UInt32

    case typeString => 
      val boxedType = NativeTypeNameToBoxedType.apply.applyOrElse(typeString, { unknownName : String =>
        throw new BadSpecialFormException(typeDatum, "Unknown native type: " + unknownName)
      })

      nfi.BoxedValue(boxedType)
  }
    
  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], returnTypeDatum : sst.ScopedSymbol, nativeSymbol : String) : et.NativeFunction = {
    var fixedArgTypes = fixedArgData map {
      case symbol : sst.ScopedSymbol =>
        parseNativeType(symbol)
      case nonsymbol =>
        throw new BadSpecialFormException(nonsymbol, "Excepted native type name to be symbol")
    }

    val hasRestArg = restArgDatum match {
      case Some(sst.ScopedSymbol(_, "boxed-list-element")) => true
      case Some(other) => 
        throw new BadSpecialFormException(other, "Only boxed-list-element can be used as a rest argument")
      case None => false
    }

    val returnType = returnTypeDatum match {
      case sst.ScopedSymbol(_, "void") => None
      case typeDatum => Some(parseNativeType(typeDatum))
    }

    et.NativeFunction(
      fixedArgs = fixedArgTypes,
      hasRestArg = hasRestArg,
      returnType = returnType,
      nativeSymbol = nativeSymbol)
  }

  def apply(operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    // These mirror the lambda forms
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedProperList(fixedArgs) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(fixedArgs, None, returnTypeDatum, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: (restArgDatum : sst.ScopedSymbol) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(Nil, Some(restArgDatum), returnTypeDatum, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedImproperList(fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: (returnTypeDatum : sst.ScopedSymbol) :: Nil =>
      createNativeFunction(fixedArgs, Some(restArgDatum), returnTypeDatum, nativeSymbol)

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native-function operands")
  }
}
