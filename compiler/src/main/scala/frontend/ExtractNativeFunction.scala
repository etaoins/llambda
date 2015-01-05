package io.llambda.compiler.frontend
import io.llambda

import scala.collection.breakOut

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

object ExtractNativeFunction {
  private def extractMonoSignature(
      hasWorldArg : Boolean,
      args : List[sst.ScopedDatum],
      procTypeDatum : sst.ScopedDatum,
      attributeData : List[sst.ScopedDatum]
  ) : ProcedureSignature = {
    val parsed = ParseProcedureTypeConstructor(procTypeDatum, args)

    val fixedArgTypes = parsed.fixedArgData.map(ExtractType.extractValueType(_))
    val restArgMemberTypeOpt = parsed.restArgMemberDatumOpt.map(ExtractType.extractSchemeType(_))
    val returnType = ExtractType.extractReturnValueType(parsed.returnDatum)

    val attributes = (attributeData.map {
      case sst.ResolvedSymbol(Primitives.NoReturnAttr) =>
        ProcedureAttribute.NoReturn

      case other =>
        throw new BadSpecialFormException(other, "Non-attribute used where procedure attribute expected")
    })(breakOut) : Set[ProcedureAttribute]

    ProcedureSignature(
      hasWorldArg=hasWorldArg,
      hasSelfArg=false,
      fixedArgTypes=fixedArgTypes,
      restArgMemberTypeOpt=restArgMemberTypeOpt,
      returnType=returnType,
      attributes=attributes
    )
  }

  private def extractPolySignature(
      hasWorldArg : Boolean,
      typeVarData : List[sst.ScopedDatum],
      args : List[sst.ScopedDatum],
      procTypeDatum : sst.ScopedDatum,
      attributeData : List[sst.ScopedDatum]
  ) : PolymorphicSignature = {
    val namedTypeVars = typeVarData map ExtractTypeVar

    // Rescope the definition
    val typeBindings = namedTypeVars map { case (name, typeVar) =>
      name -> (BoundType(typeVar) : BoundValue)
    }

    val scopeMapping = Scope.mappingForBoundValues(typeBindings)

    val rescopedArgs = args.map(_.rescoped(scopeMapping))
    val template = extractMonoSignature(hasWorldArg, rescopedArgs, procTypeDatum, attributeData)

    PolymorphicSignature(namedTypeVars.map(_._2).toSet, template)
  }


  private def extractSignature(
      hasWorldArg : Boolean,
      procTypeDatum : sst.ScopedDatum,
      attributeData : List[sst.ScopedDatum]
  ) : PolymorphicSignature = procTypeDatum match {
    case sst.ScopedProperList(sst.ResolvedSymbol(Primitives.ProcedureType) :: args) =>
      extractMonoSignature(hasWorldArg, args, procTypeDatum, attributeData).toPolymorphic

    // Long form
    case sst.ScopedProperList(List(
      sst.ResolvedSymbol(Primitives.PolymorphicType),
      sst.ScopedProperList(typeVarData),
      sst.ScopedProperList(sst.ResolvedSymbol(Primitives.ProcedureType) :: args)
    )) =>
      extractPolySignature(hasWorldArg, typeVarData, args, procTypeDatum, attributeData)

    // Shorthand
    case sst.ScopedProperList(
      sst.ResolvedSymbol(Primitives.PolymorphicType) ::
      sst.ScopedProperList(typeVarData) ::
      args
    ) =>
      extractPolySignature(hasWorldArg, typeVarData, args, procTypeDatum, attributeData)

    case _ =>
      throw new BadSpecialFormException(procTypeDatum, "Bad native function type definition")
  }

  def apply(
      hasWorldArg : Boolean,
      args : List[sst.ScopedDatum],
      defineLocation : SourceLocated
  ) : et.NativeFunction = args match {
    case libraryDatum :: sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: procTypeDatum :: attributeData =>
      val nativeLibrary = ExtractNativeLibrary(libraryDatum)

      val polySignature = extractSignature(hasWorldArg, procTypeDatum, attributeData)

      et.NativeFunction(
        library=nativeLibrary,
        polySignature=polySignature,
        nativeSymbol=nativeSymbol
      )

    case _ =>
      throw new BadSpecialFormException(defineLocation, "Bad native function symbol definition")
  }
}
