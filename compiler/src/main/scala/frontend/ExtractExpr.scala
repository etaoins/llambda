package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.frontend.syntax.ExpandMacro

private[frontend] object ExtractExpr {
  private def extractInclude(
      scope : Scope,
      includeNameData : List[sst.ScopedDatum],
      includeLocation : SourceLocated,
      foldCase : Boolean = false
  )(implicit context : FrontendContext) : et.Expr = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope), includeLocation)(context.config.includePath)

    val includeExprs = includeResults flatMap { result =>
      val innerConfig = context.config.copy(
        includePath=result.innerIncludePath
      )

      // XXX: What should we use for a subprogram here if we're including inside a lambda?
      // Clang appears to create synthetic lexical block entries just so it can attach a new filename to the source
      // lines. This likely requires a more in-depth study of the DWARF specification. For now assume we're being
      // included at the top level
      val includeSubprogram = debug.FileContext(result.filename)

      val includeContext = context.copy(config=innerConfig, debugContext=includeSubprogram)

      val data = if (foldCase) {
        result.data.map(_.toCaseFolded)
      }
      else {
        result.data
      }

      ExtractModuleBody(data, scope)(includeContext)
    }

    et.Begin(includeExprs)
  }

  private def extractSymbolApplication(
      boundValue : BoundValue,
      appliedSymbol : sst.ScopedSymbol,
      operands : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : et.Expr = {
    (boundValue, operands) match {
      case (storageLoc : StorageLocation, args) =>
        et.Apply(
          et.VarRef(storageLoc).assignLocationAndContextFrom(appliedSymbol, context.debugContext),
          args.map(extractInnerExpr)
        )

      case (Primitives.Begin, exprs) =>
        // Create a new scope using a self-executing lambda
        val beginLambda = ExtractLambda(
          located=appliedSymbol,
          argList=Nil,
          argTerminator=sst.NonSymbolLeaf(ast.EmptyList()),
          definition=exprs
        )

        beginLambda.assignLocationFrom(appliedSymbol)

        et.Apply(beginLambda, Nil)

      case (Primitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (Primitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          ExtractExpr(test),
          ExtractExpr(trueExpr),
          ExtractExpr(falseExpr))

      case (Primitives.If, test :: trueExpr :: Nil) =>
        et.Cond(
          ExtractExpr(test),
          ExtractExpr(trueExpr),
          et.Literal(ast.UnitValue()).assignLocationAndContextFrom(appliedSymbol, context.debugContext)
        )

      case (Primitives.Set, (mutatingSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, ExtractExpr(value))
          case _ =>
            throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) non-variable ${mutatingSymbol.name}")
        }

      case (Primitives.Lambda, sst.ScopedListOrDatum(fixedArgData, restArgDatum) :: definition) =>
        ExtractLambda(
          located=appliedSymbol,
          argList=fixedArgData,
          argTerminator=restArgDatum,
          definition=definition
        )

      case (Primitives.CaseLambda, clauseData) =>
        ExtractCaseLambda(
          located=appliedSymbol,
          clauseData=clauseData
        )

      case (Primitives.SyntaxError, (errorDatum @ sst.NonSymbolLeaf(ast.StringLiteral(errorString))) :: data) =>
        throw new UserDefinedSyntaxError(errorDatum, errorString, data.map(_.unscope))

      case (Primitives.Include, includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = appliedSymbol.scope
        extractInclude(scope, includeNames, appliedSymbol)

      case (Primitives.IncludeCI, includeNames) =>
        val scope = appliedSymbol.scope
        extractInclude(scope, includeNames, appliedSymbol, foldCase=true)

      case (Primitives.NativeFunction, _) =>
        ExtractNativeFunction(appliedSymbol, false, operands)

      case (Primitives.WorldFunction, _) =>
        ExtractNativeFunction(appliedSymbol, true, operands)

      case (Primitives.Quasiquote, sst.ScopedProperList(listData) :: Nil) =>
        val schemeBase = context.libraryLoader.loadSchemeBase(context.config)
        (new ListQuasiquotationExpander(extractInnerExpr, schemeBase))(listData)

      case (Primitives.Quasiquote, sst.ScopedVectorLiteral(elements) :: Nil) =>
        val schemeBase = context.libraryLoader.loadSchemeBase(context.config)
        (new VectorQuasiquotationExpander(extractInnerExpr, schemeBase))(elements.toList)

      case (Primitives.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation")

      case (Primitives.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation")

      case (Primitives.Cast, valueExpr :: typeDatum :: Nil) =>
        et.Cast(ExtractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), false)

      case (Primitives.AnnotateExprType, valueExpr :: typeDatum :: Nil) =>
        et.Cast(ExtractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), true)

      case (Primitives.CondExpand, firstClause :: restClauses) =>
        val expandedData = CondExpander.expandScopedData(firstClause :: restClauses)(context.libraryLoader, context.config)

        et.Begin(expandedData.map(extractInnerExpr))

      case (Primitives.Parameterize, sst.ScopedProperList(parameterData) :: bodyData) =>
        val parameters = parameterData map { parameterDatum =>
          parameterDatum match {
            case sst.ScopedProperList(List(parameter, value)) =>
              (ExtractExpr(parameter), ExtractExpr(value))

            case _ =>
              throw new BadSpecialFormException(parameterDatum, "Parameters must be defined as (parameter value)")
          }
        }

        et.Parameterize(
          parameterValues=parameters,
          ExtractBodyDefinition(Nil, bodyData)
        )

      case (Primitives.MakePredicate, List(typeDatum)) =>
        val nonProcType = ExtractType.extractSchemeType(typeDatum) match {
          case _ : vt.ProcedureType =>
            throw new BadSpecialFormException(typeDatum, "Creating procedure predicates it not possible; procedures of different types do not have distinct runtime representations")

          case nonProcType =>
            nonProcType
        }

        et.TypePredicate(nonProcType)

      case otherPrimitive =>
        throw new BadSpecialFormException(appliedSymbol, "Invalid primitive syntax")
    }
  }

  private def disallowTopLevelRedefinition(symbol : sst.ScopedSymbol)(implicit context : FrontendContext) {
    if (!context.config.schemeDialect.allowTopLevelRedefinition && symbol.resolveOpt.isDefined) {
      throw new DuplicateDefinitionException(symbol)
    }
  }

  private def extractApplicationLike(
      boundValue : BoundValue,
      appliedSymbol : sst.ScopedSymbol,
      operands : List[sst.ScopedDatum],
      atOutermostLevel : Boolean
  )(implicit context : FrontendContext) : et.Expr = {
    // Try to parse this as a type of definition
    ParseDefine(appliedSymbol, boundValue, operands) match {
      case Some(_) if !atOutermostLevel=>
        throw new DefinitionOutsideTopLevelException(appliedSymbol)

      case Some(ParsedVarDefine(symbol, providedTypeOpt, exprBlock, storageLocConstructor)) =>
        // There's a wart in Scheme that allows a top-level (define) to become a (set!) if the value is already defined
        // as a storage location
        symbol.resolveOpt match {
          case Some(_) if !context.config.schemeDialect.allowTopLevelRedefinition =>
            throw new DuplicateDefinitionException(symbol)

          case Some(storageLoc : StorageLocation) =>
            // Convert this to a (set!)
            et.MutateVar(storageLoc, exprBlock())

          case Some(_) =>
            throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}")

          case None =>
            // This is a fresh binding
            val schemeType = SchemeTypeForSymbol(symbol, providedTypeOpt)
            val boundValue = storageLocConstructor(symbol.name, schemeType)

            symbol.scope += (symbol.name -> boundValue)
            et.TopLevelDefine(List(et.SingleBinding(boundValue, exprBlock())))
        }

      case Some(ParsedMultipleValueDefine(fixedValueTargets, restValueTargetOpt, exprBlock)) =>
        // Don't support re-defining top-level values with (define-values) in any dialect
        for (symbol <- (fixedValueTargets ++ restValueTargetOpt).map(_.definedSymbol)) {
          if (symbol.resolveOpt.isDefined) {
            throw new DuplicateDefinitionException(symbol)
          }
        }

        val fixedLocs = fixedValueTargets.map(_.createStorageLoc(vt.AnySchemeType))
        val restLocOpt = restValueTargetOpt.map(_.createStorageLoc(vt.UniformProperListType(vt.AnySchemeType)))

        et.TopLevelDefine(List(et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock())))

      case Some(ParsedSimpleDefine(symbol, boundValue)) =>
        disallowTopLevelRedefinition(symbol)

        // This doesn't create any expression tree nodes
        symbol.scope += (symbol.name -> boundValue)
        et.Begin(Nil)

      case Some(ParsedRecordTypeDefine(typeSymbol, recordType, procedures)) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        et.TopLevelDefine((procedures.map { case (procedureSymbol, expr) =>
          val schemeType = SchemeTypeForSymbol(procedureSymbol)
          val storageLoc = new StorageLocation(procedureSymbol.name, schemeType)

          disallowTopLevelRedefinition(procedureSymbol)

          procedureSymbol.scope += (procedureSymbol.name -> storageLoc)

          et.SingleBinding(storageLoc, expr)
        }).toList)

      case Some(ParsedTypeAnnotation) =>
        et.Begin(Nil)

      case None =>
        // Apply the symbol
        // This is the only way to "apply" syntax and primitives
        // They cannot appear as normal expression values
        extractSymbolApplication(boundValue, appliedSymbol, operands)
    }
  }

  private def extractInnerExpr(datum : sst.ScopedDatum)(implicit context : FrontendContext) : et.Expr =
    ExtractExpr(datum, false)

  private def extractOutermostExpr(datum : sst.ScopedDatum)(implicit context : FrontendContext) : et.Expr =
    ExtractExpr(datum, true)

  def apply(
      datum : sst.ScopedDatum,
      atOutermostLevel : Boolean = false
  )(implicit context : FrontendContext) : et.Expr = (datum match {
    case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
      appliedSymbol.resolve match {
        case syntax : BoundSyntax =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)

          // Get the expanded expression
          val expandedContext = context.copy(debugContext=syntax.debugContext)
          val expandedExpr = ExtractExpr(expandedDatum, atOutermostLevel)(expandedContext)

          // Mark our expansion in the inline path
          // This is used for debug info and error reporting
          val inlinePathEntry = InlinePathEntry(
            locationOpt=appliedSymbol.locationOpt,
            contextOpt=Some(context.debugContext),
            inlineReason=InlineReason.MacroExpansion
          )

          expandedExpr.asInlined(inlinePathEntry)

        case otherBoundValue =>
          // Make sure the operands are a proper list
          // XXX: Does R7RS only allow macros to be applied as an improper list?
          cdr match {
            case sst.ScopedProperList(operands) =>
              extractApplicationLike(otherBoundValue, appliedSymbol, operands, atOutermostLevel)

            case improperList =>
              throw new MalformedExprException(improperList, "Non-syntax cannot be applied as an improper list")
          }
      }

    case sst.ScopedProperList(procedure :: args) =>
      // Apply the result of the inner expression
      val procedureExpr = extractInnerExpr(procedure)
      et.Apply(procedureExpr, args.map(extractInnerExpr))

    case scopedSymbol : sst.ScopedSymbol =>
      scopedSymbol.resolve match {
        case storageLoc : StorageLocation =>
          et.VarRef(storageLoc)

        case _ : BoundSyntax =>
          throw new MalformedExprException(scopedSymbol, "Syntax cannot be used as an expression")

        case _ : PrimitiveExpr =>
          throw new MalformedExprException(scopedSymbol, "Primitive cannot be used as an expression")

        case _ : TypeConstructor =>
          throw new MalformedExprException(scopedSymbol, "Type constructor cannot be used as an expression")

        case _ : BoundType =>
          throw new MalformedExprException(scopedSymbol, "Type cannot be used as an expression")

        case _ : NativeLibrary =>
          throw new MalformedExprException(scopedSymbol, "Native library cannot be used as an expression")
      }

    // These all evaluate to themselves. See R7RS section 4.1.2
    case literal : sst.ScopedVectorLiteral =>
      et.Literal(literal.unscope)
    case sst.NonSymbolLeaf(literal : ast.NumberLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal : ast.StringLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal : ast.CharLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal : ast.Bytevector) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal : ast.BooleanLiteral) =>
      et.Literal(literal)

    // Additionally treat #!unit as self-evaluating
    case sst.NonSymbolLeaf(literal : ast.UnitValue) =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExprException(malformed, malformed.toString)
  }).assignLocationAndContextFrom(datum, context.debugContext)
}
