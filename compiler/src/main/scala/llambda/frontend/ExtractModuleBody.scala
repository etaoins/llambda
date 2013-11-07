package llambda.frontend

import llambda._

import collection.mutable.ListBuffer

object ExtractModuleBody {
  abstract sealed class ParsedDefine {
    val name : sst.ScopedSymbol
    val value : BoundValue
  }

  private case class ParsedVarDefine(name : sst.ScopedSymbol, value : StorageLocation, expr : () => et.Expression) extends ParsedDefine
  private case class ParsedSyntaxDefine(name : sst.ScopedSymbol, value : BoundSyntax) extends ParsedDefine

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
    datum match {
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
    }
  }

  private def getVar(scope : Scope)(name : String) : BoundValue = {
    scope.get(name).getOrElse {
      throw new UnboundVariableException(name)
    }
  }

  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Lambda = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    case class ScopedArgument(scope : Scope, name : String, boundValue : StorageLocation)

    // Determine our arguments
    val fixedArgs = fixedArgData.map {
      case sst.ScopedSymbol(scope, name) => ScopedArgument(scope, name, new StorageLocation(name))
      case datum => throw new BadSpecialFormException("Symbol expected, found " + datum.unscope)
    }

    val restArg = restArgDatum.map { scopedSymbol  =>
      ScopedArgument(scopedSymbol.scope, scopedSymbol.name, new StorageLocation(scopedSymbol.name))
    }
    
    val allArgs = fixedArgs ++ restArg.toList

    // Find all the scopes in the definition
    val definitionScopes = definition.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ uniqueScopes(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = allArgs groupBy(_.scope)
      
    val scopeMapping = (definitionScopes map { outerScope => 
      val scopeArgs = argsForScope.getOrElse(outerScope, List())
      
      // Check for duplicate args within this scope
      val scopeArgNames = scopeArgs.map(_.name)

      if (scopeArgNames.toSet.size != scopeArgNames.size) {
        throw new BadSpecialFormException("Duplicate formal parameters: " + scopeArgNames)
      }

      val binding = collection.mutable.Map(scopeArgs.map { arg =>
        (arg.name -> arg.boundValue)
      } : _*) : collection.mutable.Map[String, BoundValue]

      val innerScope = new Scope(binding, Some(outerScope))

      (outerScope -> innerScope)
    }).toMap

    // Rescope our definition
    val scopedDefinition = definition.map(rescope(_, scopeMapping))

    val defineBuilder = new ListBuffer[ParsedDefine]

    // Split our definition is to (define)s and a body
    val bodyData = scopedDefinition.dropWhile { datum => 
      parseDefine(datum) match {
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
        Some((boundValue, exprBlock))
      case ParsedSyntaxDefine(symbol, boundValue) =>
        symbol.scope += (symbol.name -> boundValue)
        None
    }

    // Execute the expression blocks now that the scopes are prepared
    val bindings = bindingBlocks map {
      case (boundValue, exprBlock) => (boundValue -> exprBlock())
    } : List[(StorageLocation, et.Expression)]

    // Find the expressions in our body 
    val bodyExprs = bodyData.map(extractExpression) : List[et.Expression]

    // Wrap the bodyExprs in an et.Let if we're introducing bindings
    // We could easily unconditionally wrap in a possibly empty et.Let but
    // that makes the unit tests a bit unwieldly
    val boundExprs = bindings match {
      case Nil => bodyExprs
      case _ => et.Bind(bindings) :: bodyExprs
    }

    // Wrap our expressions in an et.Begin unless there's exactly one
    // This isn't required but produces more readable ETs and unit tests
    val bodyExpression = boundExprs match {
      case singleValue :: Nil => 
        singleValue
      case otherValues =>
        et.Begin(boundExprs)
    }
      
    et.Lambda(fixedArgs.map(_.boundValue), restArg.map(_.boundValue), bodyExpression)
  }
  
  private def buildQuasiquotation(builderName : String, data : Seq[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = {
    // Load (scheme base) and get our builder procedure
    val schemeBase = libraryLoader.loadSchemeBase
    val builderProc = schemeBase(builderName)

    val builderArgs = data map {
      case sst.ScopedProperList((unquote : sst.ScopedSymbol) :: unquotedDatum :: Nil) if unquote.resolve == Some(SchemePrimitives.Unquote) =>
        extractExpression(unquotedDatum)

      case quotedData =>
        // Keep this quoted
        et.Literal(quotedData.unscope)
    }

    et.Apply(et.VarRef(builderProc), builderArgs.toList)
  }

  private def extractInclude(scope : Scope, includeNameData : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope))

    val includeExprs = includeResults flatMap { result =>
      // XXX: Should we disallow body defines here in a non-body context?
      // R7RS says (include) should act like a (begin) with the contents of the
      // files. Its example definition of (begin) uses a self-executing lambda
      // which would create a body context. This seems to imply this is allowed.
      apply(result.data)(scope, libraryLoader, result.innerIncludePath)
    }

    et.Begin(includeExprs)
  }

  private def extractApplication(procedure : et.Expression, procedureDatum : sst.ScopedDatum, operands : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = {
    (procedure, operands) match {
      case (et.VarRef(syntax : BoundSyntax), operands) =>
        extractExpression(ExpandMacro(syntax, operands))

      case (et.VarRef(SchemePrimitives.Quote), innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (et.VarRef(SchemePrimitives.If), test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          extractExpression(falseExpr))
      
      case (et.VarRef(SchemePrimitives.If), test :: trueExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          et.Literal(ast.UnspecificValue))

      case (et.VarRef(SchemePrimitives.Set), sst.ScopedSymbol(scope, variableName) :: value :: Nil) =>
        getVar(scope)(variableName) match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpression(value))
          case _ =>
            throw new BadSpecialFormException(s"Attempted set! non-variable ${variableName}") 
        }

      case (et.VarRef(SchemePrimitives.Lambda), (restArgDatum : sst.ScopedSymbol) :: definition) =>
        createLambda(List(), Some(restArgDatum), definition)

      case (et.VarRef(SchemePrimitives.Lambda), sst.ScopedProperList(fixedArgData) :: definition) =>
        createLambda(fixedArgData, None, definition)

      case (et.VarRef(SchemePrimitives.Lambda), sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: definition) =>
        createLambda(fixedArgData, Some(restArgDatum), definition)

      case (et.VarRef(SchemePrimitives.SyntaxError), sst.NonSymbolLeaf(ast.StringLiteral(errorString)) :: data) =>
        throw new UserDefinedSyntaxError(errorString, data.map(_.unscope))

      case (et.VarRef(SchemePrimitives.Include), includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = procedureDatum match {
          case sst.ScopedSymbol(scope, _) =>
            scope
          case _ =>
            throw new InternalCompilerErrorException("Unable to determine scope of (include)")
        }

        extractInclude(scope, includeNames)

      case (et.VarRef(NativeFunctionPrimitives.NativeFunction), _) =>
        ExtractNativeFunction(operands)

      case (et.VarRef(SchemePrimitives.Quasiquote), sst.ScopedProperList(listData) :: Nil) => 
        buildQuasiquotation("list", listData)
      
      case (et.VarRef(SchemePrimitives.Quasiquote), sst.ScopedVectorLiteral(elements) :: Nil) => 
        buildQuasiquotation("vector", elements)
      
      case (et.VarRef(SchemePrimitives.Unquote), _) =>
          throw new BadSpecialFormException(s"Attempted (unquote) outside of quasiquotation") 
      
      case (et.VarRef(SchemePrimitives.UnquoteSplicing), _) =>
          throw new BadSpecialFormException(s"Attempted (unquote-splicing) outside of quasiquotation") 

      case _ =>
        et.Apply(procedure, operands.map(extractExpression))
    }
  }

  def extractExpression(datum : sst.ScopedDatum)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(procedure :: operands) =>
      val procedureExpr = extractExpression(procedure)
      extractApplication(procedureExpr, procedure, operands)

    case sst.ScopedSymbol(scope, variableName) =>
      et.VarRef(getVar(scope)(variableName))

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

    case malformed =>
      throw new MalformedExpressionException(malformed.toString)
  }

  private def parseSyntaxDefine(datum : sst.ScopedDatum) : ParsedSyntaxDefine = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: (symbol : sst.ScopedSymbol) ::
                         sst.ScopedProperList(
                           sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
                         ) :: Nil) =>
      val literalNames = literals.map { 
        case sst.ScopedSymbol(_, name) => name
        case nonSymbol => throw new BadSpecialFormException("Symbol expected in literal list, found " + nonSymbol)
      }
      
      val parsedRules = rules map {
        case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
          SyntaxRule(pattern, template)
        case noMatch => throw new BadSpecialFormException("Unable to parse syntax rule " + noMatch)
      }

      ParsedSyntaxDefine(symbol, new BoundSyntax(literalNames, parsedRules))

    case noMatch =>
      throw new BadSpecialFormException("Unrecognized define-syntax form " + noMatch)
  }

  private def parseDefine(datum : sst.ScopedDatum)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : Option[ParsedDefine] = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define") :: (symbol : sst.ScopedSymbol) :: value :: Nil) =>
      Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
        extractExpression(value)
      }))

    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedProperList((symbol : sst.ScopedSymbol) :: fixedArgs) :: body) =>
      Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
        createLambda(fixedArgs, None, body)
      }))
    
    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedImproperList((symbol : sst.ScopedSymbol) :: fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: body) =>
      Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
        createLambda(fixedArgs, Some(restArgDatum), body)
      }))

    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: _) =>
      Some(parseSyntaxDefine(datum))

    case sst.ScopedProperList(sst.ScopedSymbol(scope, procedureName) :: operands) =>
      scope.get(procedureName) match {
        case Some(syntax : BoundSyntax) =>
          // Cheap hack to look for macros early
          // We need to expand them in a body context if they appear in a body
          // Otherwise (define) won't work in macros
          parseDefine(ExpandMacro(syntax, operands))

        case Some(InternalPrimitives.DefineReportProcedure) =>
          operands match {
            case (symbol : sst.ScopedSymbol) :: definitionData :: Nil =>
              Some(ParsedVarDefine(symbol, new ReportProcedure(symbol.name), () => {
                extractExpression(definitionData)
              }))

            case _ =>
              throw new BadSpecialFormException("define-report-procedure requires exactly two arguments")
          }

        case _ => None
      }

    case _ => None
  }

  def apply(data : Seq[ast.Datum])(implicit evalScope : Scope, libraryLoader : LibraryLoader, includePath : IncludePath) : List[et.Expression] = data match {
    case Nil => Nil
    case datum :: tailData =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      // Try to parse this as a type of definition
      parseDefine(scopedDatum) match {
        case Some(ParsedVarDefine(symbol, boundValue, exprBlock)) =>
          // There's a wart in Scheme that allows a top-level (define) to become
          // a (set!) if the value is already defined as a storage location
          symbol.resolve match {
            case Some(storageLoc : StorageLocation) =>
              // Convert this to a (set!)
              et.MutateVar(storageLoc, exprBlock()) :: apply(tailData)
            case _  =>
              // This is a fresh binding
              // Place the rest of the body inside an et.Let
              symbol.scope += (symbol.name -> boundValue)
              et.Bind(List(boundValue -> exprBlock())) :: apply(tailData)
          }

        case Some(ParsedSyntaxDefine(symbol, boundValue)) =>
          // This doesn't create any expression tree nodes 
          symbol.scope += (symbol.name -> boundValue)
          apply(tailData)
          
        case None =>
          // This isn't a define
          extractExpression(scopedDatum) :: apply(tailData)
      }
  }
}
