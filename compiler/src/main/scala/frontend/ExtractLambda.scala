package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

import llambda.compiler.valuetype.Implicits._

object ExtractLambda {
  def apply(
      located : SourceLocated,
      operandList : List[sst.ScopedDatum],
      operandTerminator : sst.ScopedDatum,
      definition : List[sst.ScopedDatum],
      sourceNameHint : Option[String] = None,
      typeDeclaration : vt.SchemeType = vt.AnySchemeType
  )(debugContext : debug.SourceContext, libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : et.Lambda = {
    // Parse our operand list
    val parsedFormals = ParseFormals(operandList, operandTerminator)

    val signatureFixedArgTypes = parsedFormals.fixedOperands map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    val signatureRestArgMemberTypeOpt = parsedFormals.restOperandOpt map { case (symbol, typeOpt) =>
      symbol -> typeOpt.getOrElse(vt.AnySchemeType)
    }

    // Process our type declaration
    val (fixedArgTypes, restArgMemberTypeOpt, returnType) = typeDeclaration match {
      case vt.ProcedureType(fixedArgAnns, restArgAnnOpt, returnTypeAnn) =>
        if (signatureFixedArgTypes.length != fixedArgAnns.length) {
          throw new BadSpecialFormException(located, s"Procedure symbol previously declared with ${fixedArgAnns.length} fixed arguments")
        }
        
        if (!signatureRestArgMemberTypeOpt.isDefined && restArgAnnOpt.isDefined) {
          throw new BadSpecialFormException(located, s"Procedure symbol previously declared with rest argument")
        }

        if (signatureRestArgMemberTypeOpt.isDefined && !restArgAnnOpt.isDefined) {
          throw new BadSpecialFormException(located, s"Procedure symbol previously declared without rest argument")
        }

        // Combine the type declaration with the signature types
        val combinedFixedArgs = signatureFixedArgTypes.zip(fixedArgAnns) map { case ((symbol, signatureType), annType) =>
          val combinedType = signatureType & annType

          if (combinedType == vt.EmptySchemeType) {
            throw new BadSpecialFormException(symbol, s"Argument type is incompatible with previous procedure type declaration of ${annType}")
          }

          symbol -> combinedType
        }

        val combinedRestArgOpt = signatureRestArgMemberTypeOpt map { case (symbol, signatureType) =>
          val annType = restArgAnnOpt.get
          val combinedType = signatureType & annType

          if (combinedType == vt.EmptySchemeType) {
            throw new BadSpecialFormException(symbol, s"Argument type is incompatible with previous procedure type declaration of ${annType}")
          }
          
          symbol -> combinedType
        } : Option[(sst.ScopedSymbol, vt.SchemeType)]

        (combinedFixedArgs, combinedRestArgOpt, returnTypeAnn)

      case _ =>
        (signatureFixedArgTypes, signatureRestArgMemberTypeOpt, vt.ReturnType.ArbitraryValues)
    }

    val boundFixedArgs = fixedArgTypes.map { case (symbol, finalType) =>
      val storageLoc = new StorageLocation(symbol.name, finalType)
      symbol -> storageLoc
    }
    
    val boundRestArgOpt = restArgMemberTypeOpt map { case (symbol, memberType : vt.SchemeType) =>
      val storageLoc = new StorageLocation(symbol.name, vt.UniformProperListType(memberType))
      symbol -> storageLoc
    }

    // Create a subprogram for debug info purposes
    val subprogramDebugContextOpt = definition.headOption.flatMap(_.locationOpt).map { location =>
      new debug.SubprogramContext(
        parentContext=debugContext,
        filenameOpt=location.filenameOpt,
        startLine=location.line,
        sourceNameOpt=sourceNameHint
      )
    }

    val bodyDebugContext = subprogramDebugContextOpt.getOrElse(debug.UnknownContext)

    // Extract the body 
    val extractor = new ModuleBodyExtractor(bodyDebugContext, libraryLoader, frontendConfig)
    val bodyExpr = extractor.extractBodyDefinition(boundFixedArgs ++ boundRestArgOpt, definition)

    val procedureType = vt.ProcedureType(
      fixedArgTypes=fixedArgTypes.map(_._2),
      restArgMemberTypeOpt=restArgMemberTypeOpt.map(_._2),
      returnType=returnType
    )

    et.Lambda(
      schemeType=procedureType,
      fixedArgs=boundFixedArgs.map(_._2),
      restArgOpt=boundRestArgOpt.map(_._2),
      body=bodyExpr,
      debugContextOpt=subprogramDebugContextOpt
    )
  }
}
