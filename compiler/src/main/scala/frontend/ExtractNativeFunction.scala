package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def createNativeFunction(
      nativeLibrary : NativeLibrary,
      hasWorldArg : Boolean,
      fixedArgData : List[sst.ScopedDatum],
      restArgDatum : sst.ScopedDatum,
      returnTypeDatumOpt : Option[sst.ScopedDatum],
      nativeSymbol : String,
      attributes : Set[ProcedureAttribute.ProcedureAttribute]
  ) : et.NativeFunction = {
    val fixedArgTypes = fixedArgData.map(ExtractType.extractValueType(_))

    val restArgMemberTypeOpt = restArgDatum match {
      case sst.NonSymbolLeaf(ast.EmptyList()) =>
        None

      case datum =>
        Some(ExtractType.extractSchemeType(datum))
    }

    val returnType = returnTypeDatumOpt match {
      case None =>
        vt.ReturnType.SingleValue(vt.UnitType)

      case Some(returnTypeDatum) =>
        ExtractType.extractReturnType(returnTypeDatum)
    }

    val signature = ProcedureSignature(
      hasWorldArg=hasWorldArg,
      hasSelfArg=false,
      fixedArgTypes=fixedArgTypes,
      restArgMemberTypeOpt=restArgMemberTypeOpt,
      returnType=returnType,
      attributes=attributes
    )

    et.NativeFunction(
      library=nativeLibrary,
      signature=signature,
      nativeSymbol = nativeSymbol
    )
  }

  def apply(hasWorldArg : Boolean, operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    case libraryDatum :: sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: functionTypeData =>
      val nativeLibrary = ExtractNativeLibrary(libraryDatum)

      functionTypeData match {
        // These mirror the lambda forms
        case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum)) =>
          createNativeFunction(nativeLibrary, hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set())

        case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "->"), returnTypeDatum) =>
          createNativeFunction(nativeLibrary, hasWorldArg, fixedArgs, restArgDatum, Some(returnTypeDatum), nativeSymbol, Set())

        case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "noreturn")) =>
          createNativeFunction(nativeLibrary, hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set(ProcedureAttribute.NoReturn))

        case _ =>
          throw new BadSpecialFormException(defineLocation, "Bad native function type definition")
      }

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
