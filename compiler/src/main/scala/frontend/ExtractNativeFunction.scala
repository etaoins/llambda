package io.llambda.compiler.frontend
import io.llambda

import scala.collection.breakOut

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def extractSignature(
      hasWorldArg : Boolean,
      procTypeDatum : sst.ScopedDatum,
      attributeData : List[sst.ScopedDatum]
  ) : ProcedureSignature = procTypeDatum match {
    case sst.ScopedProperList(sst.ResolvedSymbol(Primitives.ProcedureType) :: operands) =>
      val parsed = ParseProcedureTypeConstructor(procTypeDatum, operands)

      val fixedArgTypes = parsed.fixedArgData.map(ExtractType.extractValueType(_))
      val restArgMemberTypeOpt = parsed.restArgMemberDatumOpt.map(ExtractType.extractSchemeType(_))
      val returnType = ExtractType.extractReturnType(parsed.returnDatum)

      val attributes = (attributeData.map {
        case sst.ResolvedSymbol(Primitives.NoReturnAttr) =>
          ProcedureAttribute.NoReturn

        case other =>
          throw new BadSpecialFormException(other, "Non-attribute used where procedure attribute expected")
      })(breakOut) : Set[ProcedureAttribute.ProcedureAttribute]

      ProcedureSignature(
        hasWorldArg=hasWorldArg,
        hasSelfArg=false,
        fixedArgTypes=fixedArgTypes,
        restArgMemberTypeOpt=restArgMemberTypeOpt,
        returnType=returnType,
        attributes=attributes
      )

    case _ =>
      throw new BadSpecialFormException(procTypeDatum, "Bad native function type definition")
  }

  def apply(
      hasWorldArg : Boolean,
      operands : List[sst.ScopedDatum],
      defineLocation : SourceLocated
  ) : et.NativeFunction = operands match {
    case libraryDatum :: sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: procTypeDatum :: attributeData =>
      val nativeLibrary = ExtractNativeLibrary(libraryDatum)

      val signature = extractSignature(hasWorldArg, procTypeDatum, attributeData)

      et.NativeFunction(
        library=nativeLibrary,
        signature=signature,
        nativeSymbol=nativeSymbol
      )

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
