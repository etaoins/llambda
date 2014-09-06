package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def createNativeFunction(
      hasWorldArg : Boolean,
      fixedArgData : List[sst.ScopedDatum],
      restArgDatum : sst.ScopedDatum,
      returnTypeDatumOpt : Option[sst.ScopedDatum],
      nativeSymbol : String,
      attributes : Set[ProcedureAttribute.ProcedureAttribute]
  ) : et.NativeFunction = {
    val fixedArgTypes = fixedArgData.map(ExtractType.extractValueType(_))

    val restArgOpt = restArgDatum match {
      case sst.NonSymbolLeaf(ast.EmptyList()) =>
        None

      case datum =>
        Some(ExtractType.extractSchemeType(datum))
    }

    val returnType = returnTypeDatumOpt match {
      case None =>
        ReturnType.SingleValue(vt.UnitType)

      case Some(returnTypeDatum) =>
        ExtractType.extractReturnType(returnTypeDatum)
    }

    val signature = ProcedureSignature(
      hasWorldArg=hasWorldArg,
      hasSelfArg=false,
      fixedArgs=fixedArgTypes,
      restArgOpt=restArgOpt,
      returnType=returnType,
      attributes=attributes
    )

    et.NativeFunction(
      signature=signature,
      nativeSymbol = nativeSymbol
    )
  }

  def apply(hasWorldArg : Boolean, operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: functionTypeData =>
        functionTypeData match {
          // These mirror the lambda forms
          case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum)) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set())
          
          case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "->"), returnTypeDatum) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, Some(returnTypeDatum), nativeSymbol, Set())
          
          case List(sst.ScopedListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "noreturn")) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set(ProcedureAttribute.NoReturn))

          case _ =>
            throw new BadSpecialFormException(defineLocation, "Bad native function type definition")
        }

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
