package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.frontend.syntax.{ParseSyntaxDefine, ExpandMacro}

import collection.mutable.ListBuffer
  
private[frontend] case class ScopedArgument(symbol : sst.ScopedSymbol, boundValue : StorageLocation)

class ModuleBodyExtractor(debugContext : debug.SourceContext, libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) {
  private def uniqueScopes(datum : sst.ScopedDatum) : Set[Scope] = {
    datum match {
      case sst.ScopedPair(car, cdr) => uniqueScopes(car) ++ uniqueScopes(cdr)
      case sst.ScopedSymbol(scope, name) => Set(scope)
      case sst.ScopedVectorLiteral(elements) => 
        elements.foldLeft(Set[Scope]()) { (scopes, element) =>
          scopes ++ uniqueScopes(element)
        }
      case leaf : sst.NonSymbolLeaf => Set()
    }
  }

  private def rescope(datum : sst.ScopedDatum, mapping : Map[Scope, Scope]) : sst.ScopedDatum = { 
    (datum match {
      case sst.ScopedPair(car, cdr) =>
        sst.ScopedPair(rescope(car, mapping), rescope(cdr, mapping))
      case sst.ScopedSymbol(scope, name) =>
        mapping.get(scope) match {
          case Some(newScope) => sst.ScopedSymbol(newScope, name)
          case _ => datum
        }

      case sst.ScopedVectorLiteral(elements) =>
        sst.ScopedVectorLiteral(elements.map(rescope(_, mapping)))
      case leaf : sst.NonSymbolLeaf => 
        leaf
    }).assignLocationFrom(datum)
  }

  private def extractBodyDefinition(arguments : List[ScopedArgument], definition : List[sst.ScopedDatum]) : et.Expr = {
    // Find all the scopes in the definition
    val definitionScopes = definition.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ uniqueScopes(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = arguments groupBy(_.symbol.scope)
      
    val scopeMapping = (definitionScopes map { outerScope => 
      val scopeArgs = argsForScope.getOrElse(outerScope, List())
      
      // Check for duplicate args within this scope
      scopeArgs.foldLeft(Set[String]()) { case (names, scopeArg) =>  
        val name = scopeArg.symbol.name

        if (names.contains(name)) {
          throw new BadSpecialFormException(scopeArg.symbol, "Duplicate formal parameter: " + name)
        }

        names + name
      }

      val binding = collection.mutable.Map(scopeArgs.map { arg =>
        (arg.symbol.name -> arg.boundValue)
      } : _*) : collection.mutable.Map[String, BoundValue]

      val innerScope = new Scope(binding, Some(outerScope))

      (outerScope -> innerScope)
    }).toMap

    // Rescope our definition
    val scopedDefinition = definition.map(rescope(_, scopeMapping))
    
    // Split our definition is to (define)s and a body
    val defineBuilder = new ListBuffer[ParsedDefine]

    val bodyData = scopedDefinition.dropWhile { datum =>
      parseDefineDatum(datum) match {
        case Some(define) =>
          defineBuilder += define
          true
        case None => false
      }
    }

    // Expand our scopes with all of the defines
    val bindingBlocks = defineBuilder.toList flatMap {
      case ParsedVarDefine(symbol, boundValue, exprBlock) =>
        symbol.scope += (symbol.name -> boundValue)
        (boundValue, exprBlock) :: Nil
      case ParsedSimpleDefine(symbol, boundValue) =>
        symbol.scope += (symbol.name -> boundValue)
        Nil
      case ParsedRecordTypeDefine(typeSymbol, recordType, procedures) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        procedures.map { case (procedureSymbol, expr) =>
          val storageLoc = new StorageLocation(procedureSymbol.name)

          procedureSymbol.scope += (procedureSymbol.name -> storageLoc)
          (storageLoc, () => expr) 
        }
    }

    // Execute the expression blocks now that the scopes are prepared
    val bindings = bindingBlocks map {
      case (boundValue, exprBlock) => (boundValue -> exprBlock())
    } : List[(StorageLocation, et.Expr)]

    // Find the expressions in our body 
    val bodyExpr = et.Expr.fromSequence(
      bodyData.map(extractExpr)
    )

    bindings match {
      case Nil => bodyExpr
      case _ => et.InternalDefinition(bindings, bodyExpr)
    }
  }

  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum], sourceNameHint : Option[String]) : et.Lambda = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a given time

    // Determine our arguments
    val fixedArgs = fixedArgData.map {
      case symbol : sst.ScopedSymbol => ScopedArgument(symbol, new StorageLocation(symbol.name))
      case datum => throw new BadSpecialFormException(datum, "Symbol expected")
    }

    val restArg = restArgDatum.map { scopedSymbol  =>
      ScopedArgument(scopedSymbol, new StorageLocation(scopedSymbol.name))
    }
    
    val allArgs = fixedArgs ++ restArg.toList

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
    val bodyExpr = extractor.extractBodyDefinition(allArgs, definition)

    et.Lambda(
      fixedArgs=fixedArgs.map(_.boundValue),
      restArg=restArg.map(_.boundValue),
      body=bodyExpr,
      debugContextOpt=subprogramDebugContextOpt
    )
  }

  private def extractInclude(scope : Scope, includeNameData : List[sst.ScopedDatum], includeLocation : SourceLocated) : et.Expr = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope), includeLocation)(frontendConfig.includePath)

    val includeExprs = includeResults flatMap { result =>
      // XXX: Should we disallow body defines here in a non-body context?
      // R7RS says (include) should act like a (begin) with the contents of the files. Its example definition of (begin)
      // uses a self-executing lambda which would create a body context. This seems to imply this is allowed.
      val innerConfig = frontendConfig.copy(
        includePath=result.innerIncludePath
      )

      // XXX: What should we use for a subprogram here if we're including inside a lambda?
      // Clang appears to create synthetic lexical block entries just so it can attach a new filename to the source 
      // lines. This likely requires a more in-depth study of the DWARF specification. For now assume we're being 
      // included at the top level
      val includeSubprogram = debug.FileContext(result.filename)

      val includeBodyExtractor = new ModuleBodyExtractor(includeSubprogram, libraryLoader, innerConfig)
      includeBodyExtractor(result.data, scope)
    }

    et.Begin(includeExprs)
  }

  private def extractSymbolApplication(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : et.Expr = {
    (boundValue, operands) match {
      case (PrimitiveExprs.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (PrimitiveExprs.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          extractExpr(test), 
          extractExpr(trueExpr), 
          extractExpr(falseExpr))
      
      case (PrimitiveExprs.If, test :: trueExpr :: Nil) =>
        et.Cond(
          extractExpr(test), 
          extractExpr(trueExpr), 
          et.Literal(ast.UnitValue()).assignLocationAndContextFrom(appliedSymbol, debugContext)
        )

      case (PrimitiveExprs.Set, (mutatingSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpr(value))
          case _ =>
            throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) non-variable ${mutatingSymbol.name}") 
        }

      case (PrimitiveExprs.Lambda, (restArgDatum : sst.ScopedSymbol) :: definition) =>
        createLambda(List(), Some(restArgDatum), definition, None)

      case (PrimitiveExprs.Lambda, sst.ScopedProperList(fixedArgData) :: definition) =>
        createLambda(fixedArgData, None, definition, None)

      case (PrimitiveExprs.Lambda, sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: definition) =>
        createLambda(fixedArgData, Some(restArgDatum), definition, None)

      case (PrimitiveExprs.SyntaxError, (errorDatum @ sst.NonSymbolLeaf(ast.StringLiteral(errorString))) :: data) =>
        throw new UserDefinedSyntaxError(errorDatum, errorString, data.map(_.unscope))

      case (PrimitiveExprs.Include, includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = appliedSymbol.scope
        extractInclude(scope, includeNames, appliedSymbol)

      case (PrimitiveExprs.NativeFunction, _) =>
        ExtractNativeFunction(false, operands, appliedSymbol)
      
      case (PrimitiveExprs.WorldFunction, _) =>
        ExtractNativeFunction(true, operands, appliedSymbol)

      case (PrimitiveExprs.Quasiquote, sst.ScopedProperList(listData) :: Nil) => 
        val schemeBase = libraryLoader.loadSchemeBase(frontendConfig)
        (new ListQuasiquotationExpander(extractExpr, schemeBase))(listData)
      
      case (PrimitiveExprs.Quasiquote, sst.ScopedVectorLiteral(elements) :: Nil) => 
        val schemeBase = libraryLoader.loadSchemeBase(frontendConfig)
        (new VectorQuasiquotationExpander(extractExpr, schemeBase))(elements.toList)
      
      case (PrimitiveExprs.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation") 
      
      case (PrimitiveExprs.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation") 

      case (storageLoc : StorageLocation, operands) =>
        et.Apply(
          et.VarRef(storageLoc).assignLocationAndContextFrom(appliedSymbol, debugContext),
          operands.map(extractExpr)
        )

      case (PrimitiveExprs.AnnotateType, valueExpr :: typeDatum :: Nil) =>
        et.Cast(extractExpr(valueExpr), DatumToValueType.toSchemeType(typeDatum))

      case (PrimitiveExprs.CondExpand, firstClause :: restClauses) =>
        val expandedData = CondExpander(firstClause :: restClauses)(libraryLoader, frontendConfig)

        et.Begin(expandedData.map(extractExpr))

      case (PrimitiveExprs.Parameterize, sst.ScopedProperList(parameterData) :: bodyData) =>
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
      
      case otherPrimitive =>
        throw new BadSpecialFormException(appliedSymbol, "Invalid primitive syntax")
    }
  }

  private def parseDefineDatum(datum : sst.ScopedDatum) : Option[ParsedDefine] = datum match {
    // Could this be define-y?
    case sst.ScopedProperList((appliedSymbol : sst.ScopedSymbol) :: operands) =>
      // Don't do a hard resolve here in case we're referencing something we haven't defined yet
      appliedSymbol.resolveOpt flatMap { boundValue =>
        parseDefine(boundValue, appliedSymbol, operands)
      }

    case _ => None
  }

  private def parseDefine(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : Option[ParsedDefine] =
    (boundValue, operands) match {
      case (PrimitiveExprs.Define, (symbol : sst.ScopedSymbol) :: value :: Nil) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          extractExpr(value)
        }))

      case (PrimitiveExprs.Define, sst.ScopedProperList((symbol : sst.ScopedSymbol) :: fixedArgs) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, None, body, Some(symbol.name)).assignLocationAndContextFrom(appliedSymbol, debugContext)
        }))
      
      case (PrimitiveExprs.Define, sst.ScopedImproperList((symbol : sst.ScopedSymbol) :: fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, Some(restArgDatum), body, Some(symbol.name)).assignLocationAndContextFrom(appliedSymbol, debugContext)
        }))

      case (PrimitiveExprs.DefineSyntax, _) =>
        Some(ParseSyntaxDefine(appliedSymbol, operands, debugContext))

      case (PrimitiveExprs.DefineRecordType, _) =>
        Some(ParseRecordTypeDefine(appliedSymbol, operands))

      case (PrimitiveExprs.DefineType, (typeAlias : sst.ScopedSymbol) :: existingTypeDatum :: Nil) =>
        Some(ParsedSimpleDefine(typeAlias, BoundType(DatumToValueType(existingTypeDatum)))) 

      case (PrimitiveExprs.DefineReportProcedure, _) =>
        operands match {
          case (symbol : sst.ScopedSymbol) :: definitionData :: Nil =>
            Some(ParsedVarDefine(symbol, new ReportProcedure(symbol.name), () => {
              extractExpr(definitionData)
            }))

          case _ =>
            throw new BadSpecialFormException(appliedSymbol, "(define-report-procedure) requires exactly two arguments")
        }

      case _ => None
  } 

  private def extractApplicationLike(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum], atOutermostLevel : Boolean) : et.Expr = {
    // Try to parse this as a type of definition
    parseDefine(boundValue, appliedSymbol, operands) match {
      case Some(_) if !atOutermostLevel=>
        throw new DefinitionOutsideTopLevelException(appliedSymbol)

      case Some(ParsedVarDefine(symbol, boundValue, exprBlock)) =>
        // There's a wart in Scheme that allows a top-level (define) to become a (set!) if the value is already defined
        // as a storage location
        symbol.resolveOpt match {
          case Some(storageLoc : StorageLocation) =>
            // Convert this to a (set!)
            et.MutateVar(storageLoc, exprBlock())

          case Some(_) =>
            throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}") 

          case None  =>
            // This is a fresh binding
            symbol.scope += (symbol.name -> boundValue)
            et.TopLevelDefinition(List(boundValue -> exprBlock()))
        }

      case Some(ParsedSimpleDefine(symbol, boundValue)) =>
        // This doesn't create any expression tree nodes 
        symbol.scope += (symbol.name -> boundValue)
        et.Begin(Nil)

      case Some(ParsedRecordTypeDefine(typeSymbol, recordType, procedures)) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        et.TopLevelDefinition((procedures.map { case (procedureSymbol, expr) =>
          val storageLoc = new StorageLocation(procedureSymbol.name)

          procedureSymbol.scope += (procedureSymbol.name -> storageLoc)
          (storageLoc, expr) 
        }).toList)

      case None =>
        // Apply the symbol
        // This is the only way to "apply" syntax and primitives
        // They cannot appear as normal expression values
        extractSymbolApplication(boundValue, appliedSymbol, operands)
    }
  }

  private def extractOutermostLevelExpr(datum : sst.ScopedDatum) : et.Expr =
    extractGenericExpr(datum, true)

  private def extractExpr(datum : sst.ScopedDatum) : et.Expr =
    // Non-body datums must either result in an expression or an exception
    extractGenericExpr(datum, false)
  
  private def extractGenericExpr(datum : sst.ScopedDatum, atOutermostLevel : Boolean) : et.Expr = (datum match {
    case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
      appliedSymbol.resolve match {
        case syntax : BoundSyntax =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum)
          
          // Get the expanded expression 
          val extractor = new ModuleBodyExtractor(syntax.debugContext, libraryLoader, frontendConfig)
          val expandedExpr = extractor.extractGenericExpr(expandedDatum, atOutermostLevel)

          // Mark our expansion in the inline path
          // This is used for debug info and error reporting
          val inlinePathEntry = InlinePathEntry(
            locationOpt=appliedSymbol.locationOpt,
            contextOpt=Some(debugContext),
            inlineReason=InlineReason.MacroExpansion
          )

          expandedExpr.asInlined(inlinePathEntry)
        
        case otherBoundValue =>
          // Make sure the operands a proper list
          // XXX: Does R7RS only allow macros to be applied as an improper list?
          cdr match {
            case sst.ScopedProperList(operands) =>
              extractApplicationLike(otherBoundValue, appliedSymbol, operands, atOutermostLevel)

            case improperList =>
              throw new MalformedExprException(improperList, "Non-syntax cannot be applied as an improper list")
          }
      }

    case sst.ScopedProperList(procedure :: operands) =>
      // Apply the result of the inner expression
      val procedureExpr = extractExpr(procedure)
      et.Apply(procedureExpr, operands.map(extractExpr))

    case scopedSymbol : sst.ScopedSymbol =>
      scopedSymbol.resolve match {
        case storageLoc : StorageLocation =>
          et.VarRef(storageLoc)

        case syntax : BoundSyntax =>
          throw new MalformedExprException(scopedSymbol, "Syntax cannot be used as an expression")

        case primitive : PrimitiveExpr =>
          throw new MalformedExprException(scopedSymbol, "Primitive cannot be used as an expression")

        case boundType : BoundType =>
          throw new MalformedExprException(scopedSymbol, "Type cannot be used as an expression")
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
  }).assignLocationAndContextFrom(datum, debugContext)

  def apply(data : List[ast.Datum], evalScope : Scope) : List[et.Expr] = data flatMap { datum => 
    // Annotate our symbols with our current scope
    val scopedDatum = sst.ScopedDatum(evalScope, datum)

    extractOutermostLevelExpr(scopedDatum) match {
      case et.Begin(Nil) => None
      case other => Some(other)
    }
  }
}
