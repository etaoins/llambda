package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

import llambda.compiler.valuetype.Implicits._

object ExtractLambda {
  private case class ReconciledTypes(
    mandatoryArgTypes: List[(sst.Symbol, vt.SchemeType)],
    optionalArgTypes: List[(sst.Symbol, vt.SchemeType)],
    restArgMemberTypeOpt: Option[(sst.Symbol, vt.SchemeType)],
    returnType: vt.ReturnType.ReturnType[vt.SchemeType],
    polymorphicType: pm.PolymorphicProcedureType
  )

  private def validateArity(
      located: SourceLocated,
      formalsMandatoryArgs: Int,
      formalsOptionalArgs: Int,
      formalsRestArg: Boolean,
      declMandatoryArgs: Int,
      declOptionalArgs: Int,
      declRestArg: Boolean
  ): Unit = {
    if (formalsMandatoryArgs != declMandatoryArgs) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared with ${declMandatoryArgs} mandatory arguments")
    }

    if (formalsOptionalArgs != declOptionalArgs) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared with ${declOptionalArgs} optional arguments")
    }

    if (!formalsRestArg && declRestArg) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared with rest argument")
    }

    if (formalsRestArg && !declRestArg) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared without rest argument")
    }
  }

  private def reconcileTypes(
      located: SourceLocated,
      parsedFormals: ParsedFormals,
      typeDeclaration: LocTypeDeclaration
  ): ReconciledTypes = {
    val formalsMandatoryArgTypes = parsedFormals.mandatoryArgs map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    val formalsOptionalArgTypes = parsedFormals.optionalArgs map { case ParsedOptional(symbol, typeOpt, _) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    val formalsRestArgMemberTypeOpt = parsedFormals.restArgOpt map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    typeDeclaration match {
      case MonomorphicDeclaration(vt.ProcedureType(mandatoryArgAnns, optionalArgAnns, restArgAnnOpt, returnTypeAnn)) =>
        validateArity(
          located=located,
          formalsMandatoryArgs=formalsMandatoryArgTypes.length,
          formalsOptionalArgs=formalsOptionalArgTypes.length,
          formalsRestArg=formalsRestArgMemberTypeOpt.isDefined,
          declMandatoryArgs=mandatoryArgAnns.length,
          declOptionalArgs=optionalArgAnns.length,
          declRestArg=restArgAnnOpt.isDefined
        )

        def combineArgTypes(symbol: sst.Symbol, signatureType: vt.SchemeType, annType: vt.SchemeType): vt.SchemeType = {
          val combinedType = signatureType & annType

          if (combinedType == vt.EmptySchemeType) {
            throw new BadSpecialFormException(symbol, s"Argument type is incompatible with previous procedure type declaration of ${annType}")
          }

          combinedType
        }

        // Combine the type declaration with the signature types
        val combinedMandatoryArgs = formalsMandatoryArgTypes.zip(mandatoryArgAnns) map {
          case ((symbol, signatureType), annType) =>
            symbol -> combineArgTypes(symbol, signatureType, annType)
        }

        val combinedOptionalArgs = formalsOptionalArgTypes.zip(optionalArgAnns) map {
          case ((symbol, signatureType), annType) =>
            symbol -> combineArgTypes(symbol, signatureType, annType)
        }

        val combinedRestArgOpt = formalsRestArgMemberTypeOpt map { case (symbol, signatureType) =>
          symbol -> combineArgTypes(symbol, signatureType, restArgAnnOpt.get)
        }: Option[(sst.Symbol, vt.SchemeType)]

        val polyType = vt.ProcedureType(
          mandatoryArgTypes=combinedMandatoryArgs.map(_._2),
          optionalArgTypes=combinedOptionalArgs.map(_._2),
          restArgMemberTypeOpt=combinedRestArgOpt.map(_._2),
          returnType=returnTypeAnn
        ).toPolymorphic

        ReconciledTypes(combinedMandatoryArgs, combinedOptionalArgs, combinedRestArgOpt, returnTypeAnn, polyType)

      case PolymorphicProcedureDeclaration(polyType @ pm.PolymorphicProcedureType(typeVars, template)) =>
        val upperBound = polyType.upperBound

        val mandatoryArgAnns = upperBound.mandatoryArgTypes
        val optionalArgAnns = upperBound.optionalArgTypes
        val restArgAnnOpt = upperBound.restArgMemberTypeOpt
        val returnTypeAnn = upperBound.returnType

        validateArity(
          located=located,
          formalsMandatoryArgs=formalsMandatoryArgTypes.length,
          formalsOptionalArgs=formalsOptionalArgTypes.length,
          formalsRestArg=formalsRestArgMemberTypeOpt.isDefined,
          declMandatoryArgs=mandatoryArgAnns.length,
          declOptionalArgs=optionalArgAnns.length,
          declRestArg=restArgAnnOpt.isDefined
        )

        def checkForArgTypes(symbol: sst.Symbol, signatureType: vt.SchemeType, annType: vt.SchemeType): vt.SchemeType = {
          if (signatureType != vt.AnySchemeType) {
            throw new BadSpecialFormException(symbol, s"Polymorphic type declarations cannot be mixed with argument type annotations")
          }

          annType
        }

        val checkedMandatoryArgs = formalsMandatoryArgTypes.zip(mandatoryArgAnns) map {
          case ((symbol, signatureType), annType) =>
            symbol -> checkForArgTypes(symbol, signatureType, annType)
        }

        val checkedOptionalArgs = formalsOptionalArgTypes.zip(optionalArgAnns) map {
          case ((symbol, signatureType), annType) =>
            symbol -> checkForArgTypes(symbol, signatureType, annType)
        }

        val checkedRestArgOpt = formalsRestArgMemberTypeOpt map { case (symbol, signatureType) =>
          symbol -> checkForArgTypes(symbol, signatureType, restArgAnnOpt.get)
        }: Option[(sst.Symbol, vt.SchemeType)]

        ReconciledTypes(checkedMandatoryArgs, checkedOptionalArgs, checkedRestArgOpt, returnTypeAnn, polyType)

      case _ =>
        val polyType = vt.ProcedureType(
          mandatoryArgTypes=formalsMandatoryArgTypes.map(_._2),
          optionalArgTypes=formalsOptionalArgTypes.map(_._2),
          restArgMemberTypeOpt=formalsRestArgMemberTypeOpt.map(_._2),
          returnType=vt.ReturnType.Reachable(vt.AnySchemeType)
        ).toPolymorphic

        ReconciledTypes(
          formalsMandatoryArgTypes,
          formalsOptionalArgTypes,
          formalsRestArgMemberTypeOpt,
          vt.ReturnType.Reachable(vt.AnySchemeType),
          polyType)
    }
  }

  def apply(
      located: SourceLocated,
      argList: List[sst.ScopedDatum],
      argTerminator: sst.ScopedDatum,
      definition: List[sst.ScopedDatum],
      sourceNameHint: Option[String] = None,
      typeDeclaration: LocTypeDeclaration = MonomorphicDeclaration(vt.AnySchemeType)
  )(implicit parentContext: FrontendContext): et.Lambda = {
    // Parse our argument list
    val parsedFormals = ParseFormals(argList, argTerminator)

    // Process our type declaration
    val reconciledTypes = reconcileTypes(located, parsedFormals, typeDeclaration)

    val boundMandatoryArgs = reconciledTypes.mandatoryArgTypes.map { case (symbol, finalType) =>
      val storageLoc = new StorageLocation(symbol.name, finalType)
      symbol -> storageLoc
    }

    val typedOptionalArgs = (reconciledTypes.optionalArgTypes zip parsedFormals.optionalArgs)
    val boundOptionalArgs = typedOptionalArgs.foldLeft(List[(sst.Symbol, et.OptionalArg)]()) {
      case (boundOptionalArgs, ((symbol, finalType), ParsedOptional(_, _, defaultDatum))) =>
        val args = boundMandatoryArgs ++ boundOptionalArgs.map { case (symbol, et.OptionalArg(storageLoc, _)) =>
          symbol -> storageLoc
        }

        val scopeMapping = Scope.mappingForBoundValues(args)
        val scopedDefault = defaultDatum.rescoped(scopeMapping)

        val storageLoc = new StorageLocation(symbol.name, finalType)
        boundOptionalArgs :+ (symbol -> et.OptionalArg(storageLoc, ExtractExpr(scopedDefault)(parentContext)))
    }

    val boundRestArgOpt = reconciledTypes.restArgMemberTypeOpt map { case (symbol, memberType: vt.SchemeType) =>
      val storageLoc = new StorageLocation(symbol.name, vt.UniformProperListType(memberType))
      symbol -> storageLoc
    }

    // Create a subprogram for debug info purposes
    val subprogramDebugContextOpt = definition.headOption.flatMap(_.locationOpt).map { location =>
      new debug.SubprogramContext(
        parentContext=parentContext.debugContext,
        filenameOpt=location.filenameOpt,
        startLocation=location,
        sourceNameOpt=sourceNameHint
      )
    }

    val bodyDebugContext = subprogramDebugContextOpt.getOrElse(debug.UnknownContext)
    val bodyContext = parentContext.copy(debugContext=bodyDebugContext)

    // Extract the body
    val args = boundMandatoryArgs ++
      boundOptionalArgs.map({ case (symbol, optArg) => symbol -> optArg.storageLoc}) ++
      boundRestArgOpt

    val bodyExpr = ExtractBodyDefinition(args, definition)(bodyContext)

    et.Lambda(
      polyType=reconciledTypes.polymorphicType,
      mandatoryArgs=boundMandatoryArgs.map(_._2),
      optionalArgs=boundOptionalArgs.map(_._2),
      restArgOpt=boundRestArgOpt.map(_._2),
      body=bodyExpr,
      debugContextOpt=subprogramDebugContextOpt
    )
  }
}
