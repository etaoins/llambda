package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  // This is similar to sst.AnyList but it will match a single datum as an empty list with a terminator
  private object ListOrDatum {
    def unapply(datum : sst.ScopedDatum) : Option[(List[sst.ScopedDatum], sst.ScopedDatum)] = datum match {
      case sst.ScopedPair(car, tail)  => 
        ListOrDatum.unapply(tail).map { case (head, terminator) =>
          (car :: head, terminator)
        }
      case nonPair => Some((Nil, nonPair))
    }
  }

  private def createNativeFunction(
      hasWorldArg : Boolean,
      fixedArgData : List[sst.ScopedDatum],
      restArgDatum : sst.ScopedDatum,
      returnTypeDatum : Option[sst.ScopedSymbol],
      nativeSymbol : String,
      attributes : Set[ProcedureAttribute]
  ) : et.NativeFunction = {
    val fixedArgTypes = fixedArgData map DatumToValueType.apply

    val hasRestArg = restArgDatum match {
      case sst.NonSymbolLeaf(ast.EmptyList()) =>
        false

      case datum =>
        DatumToValueType(datum) match {
          case vt.IntrinsicCellType(ct.ListElementCell) => true
          case _ =>
            throw new BadSpecialFormException(datum, "Only list-element-cell can be used as a rest argument")
        }
    }

    val returnType = returnTypeDatum map DatumToValueType.apply

    val signature = ProcedureSignature(
      hasWorldArg=hasWorldArg,
      hasSelfArg=false,
      fixedArgs=fixedArgTypes,
      hasRestArg=hasRestArg,
      returnType=returnType,
      attributes=attributes
    )

    et.NativeFunction(
      signature=signature,
      nativeSymbol = nativeSymbol)
  }

  def apply(hasWorldArg : Boolean, operands : List[sst.ScopedDatum], defineLocation : SourceLocated) : et.NativeFunction = operands match {
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: functionTypeData =>
        functionTypeData match {
          // These mirror the lambda forms
          case List(ListOrDatum(fixedArgs, restArgDatum)) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set())
          
          case List(ListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "->"), (returnTypeDatum : sst.ScopedSymbol)) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, Some(returnTypeDatum), nativeSymbol, Set())
          
          case List(ListOrDatum(fixedArgs, restArgDatum), sst.ScopedSymbol(_, "noreturn")) =>
            createNativeFunction(hasWorldArg, fixedArgs, restArgDatum, None, nativeSymbol, Set(ProcedureAttribute.NoReturn))

          case _ =>
            throw new BadSpecialFormException(defineLocation, "Bad native function type definition")
        }

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
