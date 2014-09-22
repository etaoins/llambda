package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

import llambda.compiler.valuetype.Implicits._

object ExtractLambda {
  def apply(
      located : SourceLocated, 
      typed : Boolean,
      operandList : List[sst.ScopedDatum],
      operandTerminator : sst.ScopedDatum,
      definition : List[sst.ScopedDatum],
      sourceNameHint : Option[String] = None,
      typeDeclaration : vt.SchemeType = vt.AnySchemeType
  )(debugContext : debug.SourceContext, libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : et.Lambda = {
    // Parse our operand list in to its basic parts
    val (fixedArgData, restArgNameOpt, restArgMemberType) = 
      (operandList.reverse, operandTerminator) match {
        // This looks for a terminal rest arg in the form: name : <type> *
        case (
            sst.ScopedSymbol(_, "*") ::
            (restArgType : sst.ScopedSymbol) ::
            (typeColon @ sst.ScopedSymbol(_, ":")) ::
            (restArgName : sst.ScopedSymbol) :: 
            reverseFixedArgs
        , sst.NonSymbolLeaf(ast.EmptyList())) if typed =>
          // This is a typed rest argument 
          (reverseFixedArgs.reverse, Some(restArgName), ExtractType.extractNonEmptySchemeType(restArgType))

        case (_, restArgSymbol : sst.ScopedSymbol) =>
          // This has an untyped rest argument
          (operandList, Some(restArgSymbol), vt.AnySchemeType)
        
        case (_, sst.NonSymbolLeaf(ast.EmptyList())) =>
          // This has no rest argument
          (operandList, None, vt.AnySchemeType)
        
        case (_, datum) =>
          throw new BadSpecialFormException(datum, "Rest argument expected")
      }

    // Find the types in our signature
    val signatureFixedArgTypes = if (typed) {
      fixedArgData.map {
        case sst.ScopedProperList(List(scopedSymbol : sst.ScopedSymbol, sst.ScopedSymbol(_, ":"), typeDatum)) =>
          scopedSymbol -> ExtractType.extractNonEmptySchemeType(typeDatum)

        case other =>
          throw new BadSpecialFormException(other, "Expected (symbol : <type>)")
      }
    }
    else {
      fixedArgData.map {
        case scopedSymbol : sst.ScopedSymbol =>
          scopedSymbol -> vt.AnySchemeType

        case datum => 
          throw new BadSpecialFormException(datum, "Symbol expected")
      }
    }

    val signatureRestArgMemberTypeOpt = restArgNameOpt map { restArgName =>
      restArgName -> restArgMemberType
    } : Option[(sst.ScopedSymbol, vt.SchemeType)]

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
