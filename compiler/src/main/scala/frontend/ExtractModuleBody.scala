package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.frontend.syntax.{ParseSyntaxDefine, ExpandMacro}

import collection.mutable.ListBuffer

object ExtractModuleBody {
  private def symbolTypeDeclaration(
      symbol : sst.ScopedSymbol,
      providedTypeOpt : Option[LocTypeDeclaration] = None,
      defaultType : vt.SchemeType = vt.AnySchemeType
  ) : LocTypeDeclaration = {
    symbol.scope.typeDeclarations.get(symbol) match {
      case Some(declaredType) =>
        // Does the declared type match the provided type exactly?
        providedTypeOpt match {
          case Some(incompatibleType) if incompatibleType != declaredType =>
            throw new BadSpecialFormException(symbol, s"Symbol previously declared with type ${declaredType}")

          case _ =>
        }

        declaredType

      case None =>
        // No type declaration
        providedTypeOpt.getOrElse(MonomorphicDeclaration(defaultType))
    }
  }

  private def declaredSymbolSchemeType(
      symbol : sst.ScopedSymbol,
      providedTypeOpt : Option[vt.SchemeType] = None,
      defaultType : vt.SchemeType = vt.AnySchemeType
  ) : vt.SchemeType =
    symbolTypeDeclaration(symbol, providedTypeOpt.map(MonomorphicDeclaration), defaultType) match {
      case MonomorphicDeclaration(schemeType) =>
        schemeType

      case PolymorphicProcedureDeclaration(polyType) =>
        polyType.upperBound
    }

  private def valueTargetToLoc(valueTarget : ValueTarget, defaultType : vt.SchemeType) : StorageLocation =
    valueTarget match {
      case ValueTarget(symbol, providedTypeOpt) =>
        val schemeType = declaredSymbolSchemeType(symbol, providedTypeOpt, defaultType)
        val boundValue = new StorageLocation(symbol.name, schemeType)

        symbol.scope += (symbol.name -> boundValue)

        boundValue
    }

  /** Expands a datum in an outermost or body context
    *
    * This expands any top-level macros and flattens any (begin)s
    */
  private def expandBodyDatum(datum : sst.ScopedDatum)(implicit context : FrontendContext) : List[sst.ScopedDatum] =
    datum match {
      case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
        (appliedSymbol.resolveOpt, cdr) match {
          case (Some(syntax : BoundSyntax), _) =>
            // This is a macro - expand it
            val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)
            expandBodyDatum(expandedDatum)

          case (Some(Primitives.Begin), sst.ScopedProperList(innerExprs)) =>
            // This is a (begin) - flatten it
            innerExprs.flatMap(expandBodyDatum)

          case _ => List(datum)
        }

      case _ => List(datum)
    }

  private[frontend] def extractBodyDefinition(
      args : List[(sst.ScopedSymbol, BoundValue)],
      definition : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : et.Expr = {
    // Find all the scopes in the definition
    val definitionScopes = definition.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ UniqueScopesForDatum(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = args groupBy(_._1.scope)

    val scopeMapping = (definitionScopes map { outerScope =>
      val scopeArgs = argsForScope.getOrElse(outerScope, List())

      // Check for duplicate args within this scope
      scopeArgs.foldLeft(Set[String]()) { case (names, (symbol, _)) =>
        val name = symbol.name

        if (names.contains(name)) {
          throw new BadSpecialFormException(symbol, "Duplicate formal parameter: " + name)
        }

        names + name
      }

      val binding = collection.mutable.Map(scopeArgs.map { case (symbol, boundValue) =>
        (symbol.name -> boundValue)
      } : _*) : collection.mutable.Map[String, BoundValue]

      val innerScope = new Scope(binding, Some(outerScope))

      (outerScope -> innerScope)
    }).toMap

    // Rescope our definition
    val scopedDefinition = definition.map(_.rescoped(scopeMapping))

    // Expand any macros or (begin)s recursively
    val expandedDefinition = scopedDefinition.flatMap(expandBodyDatum)

    // Split our definition is to (define)s and a body
    val defineBuilder = new ListBuffer[ParsedDefine]

    val bodyData = expandedDefinition.dropWhile { datum =>
      parseDefineDatum(datum) match {
        case Some(define) =>
          defineBuilder += define
          true
        case None => false
      }
    }

    // Expand our scopes with all of the defines
    val bindingBlocks = defineBuilder.toList flatMap {
      case ParsedVarDefine(symbol, providedTypeOpt, exprBlock, storageLocConstructor) =>
        val schemeType = declaredSymbolSchemeType(symbol, providedTypeOpt)
        val boundValue = storageLocConstructor(symbol.name, schemeType)

        symbol.scope += (symbol.name -> boundValue)

        List(
          { () => et.SingleBinding(boundValue, exprBlock()) }
        )
      case ParsedMultipleValueDefine(fixedValueTargets, restValueTargetOpt, exprBlock) =>
        val fixedLocs = fixedValueTargets.map(valueTargetToLoc(_, vt.AnySchemeType))
        val restLocOpt = restValueTargetOpt.map(valueTargetToLoc(_, vt.UniformProperListType(vt.AnySchemeType)))

        List(
          { () => et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock()) }
        )

      case ParsedSimpleDefine(symbol, boundValue) =>
        symbol.scope += (symbol.name -> boundValue)
        Nil
      case ParsedRecordTypeDefine(typeSymbol, recordType, procedures) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        procedures.map { case (procedureSymbol, expr) =>
          val schemeType = declaredSymbolSchemeType(procedureSymbol)
          val storageLoc = new StorageLocation(procedureSymbol.name, schemeType)

          procedureSymbol.scope += (procedureSymbol.name -> storageLoc)

          { () => et.SingleBinding(storageLoc, expr) }
        }
      case ParsedTypeAnnotation =>
        Nil
    } : List[() => et.Binding]

    // Execute the expression blocks now that the scopes are prepared
    val bindings = bindingBlocks.map(_.apply)

    // Find the expressions in our body
    val bodyExpr = et.Expr.fromSequence(
      bodyData.map(extractExpr)
    )

    // Finish the inner scopes
    for((_, innerScope) <- scopeMapping) {
      FinishScope(innerScope)
    }

    bindings match {
      case Nil => bodyExpr
      case _ => et.InternalDefine(bindings, bodyExpr)
    }
  }

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
          extractExpr(test),
          extractExpr(trueExpr),
          extractExpr(falseExpr))

      case (Primitives.If, test :: trueExpr :: Nil) =>
        et.Cond(
          extractExpr(test),
          extractExpr(trueExpr),
          et.Literal(ast.UnitValue()).assignLocationAndContextFrom(appliedSymbol, context.debugContext)
        )

      case (Primitives.Set, (mutatingSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpr(value))
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
        (new ListQuasiquotationExpander(extractExpr, schemeBase))(listData)

      case (Primitives.Quasiquote, sst.ScopedVectorLiteral(elements) :: Nil) =>
        val schemeBase = context.libraryLoader.loadSchemeBase(context.config)
        (new VectorQuasiquotationExpander(extractExpr, schemeBase))(elements.toList)

      case (Primitives.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation")

      case (Primitives.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation")

      case (storageLoc : StorageLocation, args) =>
        et.Apply(
          et.VarRef(storageLoc).assignLocationAndContextFrom(appliedSymbol, context.debugContext),
          args.map(extractExpr)
        )

      case (Primitives.Cast, valueExpr :: typeDatum :: Nil) =>
        et.Cast(extractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), false)

      case (Primitives.AnnotateExprType, valueExpr :: typeDatum :: Nil) =>
        et.Cast(extractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), true)

      case (Primitives.CondExpand, firstClause :: restClauses) =>
        val expandedData = CondExpander.expandScopedData(firstClause :: restClauses)(context.libraryLoader, context.config)

        et.Begin(expandedData.map(extractExpr))

      case (Primitives.Parameterize, sst.ScopedProperList(parameterData) :: bodyData) =>
        val parameters = parameterData map { parameterDatum =>
          parameterDatum match {
            case sst.ScopedProperList(List(parameter, value)) =>
              (extractExpr(parameter), extractExpr(value))

            case _ =>
              throw new BadSpecialFormException(parameterDatum, "Parameters must be defined as (parameter value)")
          }
        }

        et.Parameterize(
          parameterValues=parameters,
          extractBodyDefinition(Nil, bodyData)
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

  private def parseDefineDatum(datum : sst.ScopedDatum)(implicit context : FrontendContext) : Option[ParsedDefine] =
    datum match {
      case sst.ScopedProperList((appliedSymbol : sst.ScopedSymbol) :: operands) =>
        appliedSymbol.resolveOpt match {
          case Some(otherBoundValue) =>
            parseDefine(otherBoundValue, appliedSymbol, operands)

          case _ => None
        }

      case _ => None
    }

  private def parseDefine(
      boundValue : BoundValue,
      appliedSymbol : sst.ScopedSymbol,
      operands : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : Option[ParsedDefine] =
    (boundValue, operands) match {
      case (Primitives.Define, List(symbol : sst.ScopedSymbol, value)) =>
        Some(ParsedVarDefine(symbol, None, () => {
          extractExpr(value)
        }))

      case (Primitives.Define, List(
          symbol : sst.ScopedSymbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          value
      )) =>
        val providedType = ExtractType.extractStableType(typeDatum)(context.config)

        Some(ParsedVarDefine(symbol, Some(providedType), () => {
          extractExpr(value)
        }))

      case (Primitives.Define, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        Some(ParsedVarDefine(symbol, None, () => {
          ExtractLambda(
            located=appliedSymbol,
            argList=fixedArgs,
            argTerminator=restArgDatum,
            definition=body,
            sourceNameHint=Some(symbol.name),
            typeDeclaration=symbolTypeDeclaration(symbol)
          ).assignLocationAndContextFrom(appliedSymbol, context.debugContext)
        }))

      case (Primitives.DefineValues, List(sst.ScopedListOrDatum(operands, operandTerminator), initialiser)) =>
        Some(parseMultipleValueDefine(operands, operandTerminator, initialiser))

      case (Primitives.DefineSyntax, _) =>
        Some(ParseSyntaxDefine(appliedSymbol, operands, context.debugContext))

      case (Primitives.DefineRecordType, _) =>
        Some(ParseRecordTypeDefine(appliedSymbol, operands)(context.config))

      case (Primitives.DefineType, (typeAlias : sst.ScopedSymbol) :: typeDatum :: Nil) =>
        // Allow the type's new name to be a recursion marker inside the definition
        val recursiveVars = ExtractType.RecursiveVars(Map(typeAlias.name -> 0))
        val extractedType = ExtractType.extractValueType(typeDatum, recursiveVars)

        Some(ParsedSimpleDefine(typeAlias, BoundType(extractedType)))

      case (Primitives.DefineType, sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: args) :: definition :: Nil) =>
        val typeConstructor = ExtractUserDefinedTypeConstructor(args, definition)
        Some(ParsedSimpleDefine(constructorName, typeConstructor))

      case (Primitives.DefineReportProcedure, List(symbol : sst.ScopedSymbol, definitionData)) =>
        Some(ParsedVarDefine(
          definedSymbol=symbol,
          providedType=None,
          expr=() => {
            extractExpr(definitionData)
          },
          storageLocConstructor=(new ReportProcedure(_, _))
        ))

      case (Primitives.DefineReportProcedure, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        Some(ParsedVarDefine(
          definedSymbol=symbol,
          providedType=None,
          expr=() => {
            ExtractLambda(
              located=appliedSymbol,
              argList=fixedArgs,
              argTerminator=restArgDatum,
              definition=body,
              sourceNameHint=Some(symbol.name),
              typeDeclaration=symbolTypeDeclaration(symbol)
            ).assignLocationAndContextFrom(appliedSymbol, context.debugContext)
          },
          storageLocConstructor=(new ReportProcedure(_, _))
        ))

      case (Primitives.DefineNativeLibrary, List(libAlias : sst.ScopedSymbol, libDatum)) =>
        Some(ParsedSimpleDefine(libAlias, ExtractNativeLibrary(libDatum)))

      case (Primitives.AnnotateStorageLocType, List(declaredSymbol : sst.ScopedSymbol, typeDatum)) =>
        val declarationType = ExtractType.extractLocTypeDeclaration(typeDatum)

        declaredSymbol.scope.bindings.get(declaredSymbol.name) match {
          case None =>
            // No previous binding
          case Some(compatibleLoc : StorageLocation) if MonomorphicDeclaration(compatibleLoc.schemeType) == declarationType =>
            // Previous binding with compatible type
          case _ =>
            throw new BadSpecialFormException(declaredSymbol, "Symbol previously defined with incompatible value")
        }

        // Make sure there have been no incompatible declarations before
        symbolTypeDeclaration(declaredSymbol, Some(declarationType))

        // Record this declaration
        declaredSymbol.scope.typeDeclarations += (declaredSymbol -> declarationType)

        Some(ParsedTypeAnnotation)

      case _ => None
  }

  def parseMultipleValueDefine(
      argList : List[sst.ScopedDatum],
      argTerminator : sst.ScopedDatum,
      initialiserDatum : sst.ScopedDatum
  )(implicit context : FrontendContext) : ParsedMultipleValueDefine = {
    val parsedFormals = ParseFormals(argList, argTerminator)

    val fixedValueTargets = parsedFormals.fixedArgs map { case (symbol, schemeTypeOpt) =>
      ValueTarget(symbol, schemeTypeOpt)
    }

    val restValueTargetOpt = parsedFormals.restArgOpt map { case (symbol, memberTypeOpt) =>
      ValueTarget(symbol, memberTypeOpt.map(vt.UniformProperListType(_)))
    }

    ParsedMultipleValueDefine(
      fixedValueTargets=fixedValueTargets,
      restValueTargetOpt=restValueTargetOpt,
      expr=() => {extractExpr(initialiserDatum) }
    )
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
    parseDefine(boundValue, appliedSymbol, operands) match {
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
            val schemeType = declaredSymbolSchemeType(symbol, providedTypeOpt)
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

        val fixedLocs = fixedValueTargets.map(valueTargetToLoc(_, vt.AnySchemeType))
        val restLocOpt = restValueTargetOpt.map(valueTargetToLoc(_, vt.UniformProperListType(vt.AnySchemeType)))

        et.TopLevelDefine(List(et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock())))

      case Some(ParsedSimpleDefine(symbol, boundValue)) =>
        disallowTopLevelRedefinition(symbol)

        // This doesn't create any expression tree nodes
        symbol.scope += (symbol.name -> boundValue)
        et.Begin(Nil)

      case Some(ParsedRecordTypeDefine(typeSymbol, recordType, procedures)) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        et.TopLevelDefine((procedures.map { case (procedureSymbol, expr) =>
          val schemeType = declaredSymbolSchemeType(procedureSymbol)
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

  private def extractOutermostLevelExpr(datum : sst.ScopedDatum)(implicit context : FrontendContext) : et.Expr =
    extractGenericExpr(datum, true)

  private def extractExpr(datum : sst.ScopedDatum)(implicit context : FrontendContext)  : et.Expr =
    // Non-body datums must either result in an expression or an exception
    extractGenericExpr(datum, false)

  private def extractGenericExpr(
      datum : sst.ScopedDatum,
      atOutermostLevel : Boolean
  )(implicit context : FrontendContext) : et.Expr = (datum match {
    case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
      appliedSymbol.resolve match {
        case syntax : BoundSyntax =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)

          // Get the expanded expression
          val expandedContext = context.copy(debugContext=syntax.debugContext)
          val expandedExpr = extractGenericExpr(expandedDatum, atOutermostLevel)(expandedContext)

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
      val procedureExpr = extractExpr(procedure)
      et.Apply(procedureExpr, args.map(extractExpr))

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

  def apply(data : List[ast.Datum], evalScope : Scope)(implicit context : FrontendContext) : List[et.Expr] =
    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      expandBodyDatum(scopedDatum).flatMap {
        extractOutermostLevelExpr(_).toSequence
      }
    }
}
