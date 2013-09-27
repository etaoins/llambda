package llambda.frontend

import llambda._

object ExtractNativeFunction
{
  private def parseNativeType(typeString : String) : nfi.NativeType = typeString match {
    case "bool" => nfi.Bool
    case "int8"   => nfi.Int8
    case "int16"  => nfi.Int16
    case "int32"  => nfi.Int32
    case "int64"  => nfi.Int64
    case "uint8"  => nfi.UInt8
    case "uint16" => nfi.UInt16
    case "uint32" => nfi.UInt32
    case "uint64" => nfi.UInt64
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
    case "ulong"  => nfi.UInt64

    case _ => 
      val boxedType = NativeTypeNameToBoxedType.apply.applyOrElse(typeString, { unknownName : String =>
        throw new BadSpecialFormException("Unknown native type: " + unknownName)
      })

      nfi.BoxedValue(boxedType)
  }
    
  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgType : Option[String], returnTypeString : String, nativeSymbol : String) : et.NativeFunction = {
    var fixedArgTypes = fixedArgData map {
      case sst.ScopedSymbol(_, typeName) => parseNativeType(typeName)
      case nonsymbol => throw new BadSpecialFormException("Excepted native type name to be string: " + nonsymbol)
    }

    val hasRestArg = restArgType match {
      case Some("boxed-pair") => true
      case Some(other) => throw new BadSpecialFormException("Only boxed-datum can be used as a rest argument. Found: " + other)
      case None => false
    }

    val returnType = returnTypeString match {
      case "void" => None
      case _ => Some(parseNativeType(returnTypeString))
    }

    et.NativeFunction(
      fixedArgs = fixedArgTypes,
      hasRestArg = hasRestArg,
      returnType = returnType,
      nativeSymbol = nativeSymbol)
  }

  def apply(operands : List[sst.ScopedDatum]) : et.NativeFunction = operands match {
    // These mirror the lambda forms
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedProperList(fixedArgs) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(fixedArgs, None, returnType, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedSymbol(_, restArgType) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(Nil, Some(restArgType), returnType, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedImproperList(fixedArgs, sst.ScopedSymbol(_, restArgType)) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(fixedArgs, Some(restArgType), returnType, nativeSymbol)

    case _ =>
      throw new BadSpecialFormException("Bad native-function operands: " + operands.mkString(" "))
  }
}
