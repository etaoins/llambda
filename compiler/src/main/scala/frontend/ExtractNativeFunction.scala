package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def createNativeFunction(hasWorldArg : Boolean, fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], returnTypeDatum : Option[sst.ScopedSymbol], nativeSymbol : String) : et.NativeFunction = {
    val fixedArgTypes = fixedArgData map DatumToValueType.apply

    val hasRestArg = restArgDatum match {
      case Some(datum) =>
        DatumToValueType(datum) match {
          case vt.IntrinsicCellType(ct.ListElementCell) => true
          case _ =>
            throw new BadSpecialFormException(datum, "Only list-element-cell can be used as a rest argument")
        }

      case None => false
    }

    val returnType = returnTypeDatum map DatumToValueType.apply

    val signature = ProcedureSignature(
      hasWorldArg=hasWorldArg,
      hasSelfArg=false,
      fixedArgs=fixedArgTypes,
      hasRestArg=hasRestArg,
      returnType=returnType
    )

    et.NativeFunction(
      signature=signature,
      nativeSymbol = nativeSymbol)
  }

  def apply(hasWorldArg : Boolean, operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: functionTypeData =>
        functionTypeData match {
          // These mirror the lambda forms
          case List(sst.ScopedProperList(fixedArgs)) =>
            createNativeFunction(hasWorldArg, fixedArgs, None, None, nativeSymbol)

          case List(sst.ScopedProperList(fixedArgs), sst.ScopedSymbol(_, "->"), returnTypeDatum : sst.ScopedSymbol) =>
            createNativeFunction(hasWorldArg, fixedArgs, None, Some(returnTypeDatum), nativeSymbol)
          
          case List(restArgDatum : sst.ScopedSymbol) =>
            createNativeFunction(hasWorldArg, Nil, Some(restArgDatum), None, nativeSymbol)
          
          case List(restArgDatum : sst.ScopedSymbol, sst.ScopedSymbol(_, "->"), (returnTypeDatum : sst.ScopedSymbol)) =>
            createNativeFunction(hasWorldArg, Nil, Some(restArgDatum), Some(returnTypeDatum), nativeSymbol)
          
          case List(sst.ScopedImproperList(fixedArgs, restArgDatum : sst.ScopedSymbol)) =>
            createNativeFunction(hasWorldArg, fixedArgs, Some(restArgDatum), None, nativeSymbol)
          
          case List(sst.ScopedImproperList(fixedArgs, restArgDatum : sst.ScopedSymbol), sst.ScopedSymbol(_, "->"), (returnTypeDatum : sst.ScopedSymbol)) =>
            createNativeFunction(hasWorldArg, fixedArgs, Some(restArgDatum), Some(returnTypeDatum), nativeSymbol)

          case _ =>
            throw new BadSpecialFormException(defineLocation, "Bad native function type definition")
        }

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
