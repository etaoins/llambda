package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

import llambda.compiler.valuetype.Implicits._

object ExtractLambda {
  private case class ReconciledTypes(
    fixedArgTypes : List[(sst.ScopedSymbol, vt.SchemeType)],
    restArgMemberTypeOpt : Option[(sst.ScopedSymbol, vt.SchemeType)],
    returnType : vt.ReturnType.ReturnType[vt.SchemeType],
    polymorphicType : pm.PolymorphicProcedureType
  )

  private def validateArity(
      located : SourceLocated,
      formalsFixedArgs : Int,
      formalsRestArg : Boolean,
      declFixedArgs : Int,
      declRestArg : Boolean
  ) : Unit = {
    if (formalsFixedArgs != declFixedArgs) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared with ${declFixedArgs} fixed arguments")
    }

    if (!formalsRestArg && declRestArg) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared with rest argument")
    }

    if (formalsRestArg && !declRestArg) {
      throw new BadSpecialFormException(located, s"Procedure symbol previously declared without rest argument")
    }
  }

  private def reconcileTypes(
      located : SourceLocated,
      parsedFormals : ParsedFormals,
      typeDeclaration : LocTypeDeclaration
  ) : ReconciledTypes = {
    val formalsFixedArgTypes = parsedFormals.fixedArgs map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    val formalsRestArgMemberTypeOpt = parsedFormals.restArgOpt map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    typeDeclaration match {
      case MonomorphicDeclaration(vt.ProcedureType(fixedArgAnns, restArgAnnOpt, returnTypeAnn)) =>
        validateArity(
          located=located,
          formalsFixedArgs=formalsFixedArgTypes.length,
          formalsRestArg=formalsRestArgMemberTypeOpt.isDefined,
          declFixedArgs=fixedArgAnns.length,
          declRestArg=restArgAnnOpt.isDefined
        )

        def combineArgTypes(symbol : sst.ScopedSymbol, signatureType : vt.SchemeType, annType : vt.SchemeType) : vt.SchemeType = {
          val combinedType = signatureType & annType

          if (combinedType == vt.EmptySchemeType) {
            throw new BadSpecialFormException(symbol, s"Argument type is incompatible with previous procedure type declaration of ${annType}")
          }

          combinedType
        }

        // Combine the type declaration with the signature types
        val combinedFixedArgs = formalsFixedArgTypes.zip(fixedArgAnns) map { case ((symbol, signatureType), annType) =>
          symbol -> combineArgTypes(symbol, signatureType, annType)
        }

        val combinedRestArgOpt = formalsRestArgMemberTypeOpt map { case (symbol, signatureType) =>
          symbol -> combineArgTypes(symbol, signatureType, restArgAnnOpt.get)
        } : Option[(sst.ScopedSymbol, vt.SchemeType)]

        val polyType = vt.ProcedureType(
          fixedArgTypes=combinedFixedArgs.map(_._2),
          restArgMemberTypeOpt=combinedRestArgOpt.map(_._2),
          returnType=returnTypeAnn
        ).toPolymorphic

        ReconciledTypes(combinedFixedArgs, combinedRestArgOpt, returnTypeAnn, polyType)

      case PolymorphicProcedureDeclaration(polyType @ pm.PolymorphicProcedureType(typeVars, template)) =>
        val upperBound = polyType.upperBound

        val fixedArgAnns = upperBound.fixedArgTypes
        val restArgAnnOpt = upperBound.restArgMemberTypeOpt
        val returnTypeAnn = upperBound.returnType

        validateArity(
          located=located,
          formalsFixedArgs=formalsFixedArgTypes.length,
          formalsRestArg=formalsRestArgMemberTypeOpt.isDefined,
          declFixedArgs=fixedArgAnns.length,
          declRestArg=restArgAnnOpt.isDefined
        )

        def checkForArgTypes(symbol : sst.ScopedSymbol, signatureType : vt.SchemeType, annType : vt.SchemeType) : vt.SchemeType = {
          if (signatureType != vt.AnySchemeType) {
            throw new BadSpecialFormException(symbol, s"Polymorphic type declarations cannot be mixed with argument type annotations")
          }

          annType
        }

        val checkedFixedArgs = formalsFixedArgTypes.zip(fixedArgAnns) map { case ((symbol, signatureType), annType) =>
          symbol -> checkForArgTypes(symbol, signatureType, annType)
        }

        val checkedRestArgOpt = formalsRestArgMemberTypeOpt map { case (symbol, signatureType) =>
          symbol -> checkForArgTypes(symbol, signatureType, restArgAnnOpt.get)
        } : Option[(sst.ScopedSymbol, vt.SchemeType)]

        ReconciledTypes(checkedFixedArgs, checkedRestArgOpt, returnTypeAnn, polyType)

      case _ =>
        val polyType = vt.ProcedureType(
          fixedArgTypes=formalsFixedArgTypes.map(_._2),
          restArgMemberTypeOpt=formalsRestArgMemberTypeOpt.map(_._2),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic

        ReconciledTypes(formalsFixedArgTypes, formalsRestArgMemberTypeOpt, vt.ReturnType.ArbitraryValues, polyType)
    }
  }

  def apply(
      located : SourceLocated,
      argList : List[sst.ScopedDatum],
      argTerminator : sst.ScopedDatum,
      definition : List[sst.ScopedDatum],
      sourceNameHint : Option[String] = None,
      typeDeclaration : LocTypeDeclaration = MonomorphicDeclaration(vt.AnySchemeType)
  )(debugContext : debug.SourceContext, libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : et.Lambda = {
    // Parse our argument list
    val parsedFormals = ParseFormals(argList, argTerminator)

    // Process our type declaration
    val reconciledTypes = reconcileTypes(located, parsedFormals, typeDeclaration)

    val boundFixedArgs = reconciledTypes.fixedArgTypes.map { case (symbol, finalType) =>
      val storageLoc = new StorageLocation(symbol.name, finalType)
      symbol -> storageLoc
    }

    val boundRestArgOpt = reconciledTypes.restArgMemberTypeOpt map { case (symbol, memberType : vt.SchemeType) =>
      val storageLoc = new StorageLocation(symbol.name, vt.UniformProperListType(memberType))
      symbol -> storageLoc
    }

    // Create a subprogram for debug info purposes
    val subprogramDebugContextOpt = definition.headOption.flatMap(_.locationOpt).map { location =>
      new debug.SubprogramContext(
        parentContext=debugContext,
        filenameOpt=location.filenameOpt,
        startLocation=location,
        sourceNameOpt=sourceNameHint
      )
    }

    val bodyDebugContext = subprogramDebugContextOpt.getOrElse(debug.UnknownContext)

    // Extract the body
    val extractor = new ModuleBodyExtractor(bodyDebugContext, libraryLoader, frontendConfig)
    val bodyExpr = extractor.extractBodyDefinition(boundFixedArgs ++ boundRestArgOpt, definition)

    et.Lambda(
      polyType=reconciledTypes.polymorphicType,
      fixedArgs=boundFixedArgs.map(_._2),
      restArgOpt=boundRestArgOpt.map(_._2),
      body=bodyExpr,
      debugContextOpt=subprogramDebugContextOpt
    )
  }
}
