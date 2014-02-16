package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], returnTypeDatum : Option[sst.ScopedSymbol], nativeSymbol : String) : et.NativeFunction = {
    var fixedArgTypes = fixedArgData map DatumToValueType.apply

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
      hasSelfArg=false,
      fixedArgs=fixedArgTypes,
      hasRestArg=hasRestArg,
      returnType=returnType
    )

    et.NativeFunction(
      signature=signature,
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
