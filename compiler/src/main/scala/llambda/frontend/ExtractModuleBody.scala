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

  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Lambda = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    case class ScopedArgument(symbol : sst.ScopedSymbol, boundValue : StorageLocation)

    // Determine our arguments
    val fixedArgs = fixedArgData.map {
      case symbol : sst.ScopedSymbol => ScopedArgument(symbol, new StorageLocation(symbol.name))
      case datum => throw new BadSpecialFormException(datum, "Symbol expected")
    }

    val restArg = restArgDatum.map { scopedSymbol  =>
      ScopedArgument(scopedSymbol, new StorageLocation(scopedSymbol.name))
    }
    
    val allArgs = fixedArgs ++ restArg.toList

    // Find all the scopes in the definition
    val definitionScopes = definition.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ uniqueScopes(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = allArgs groupBy(_.symbol.scope)
      
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

  private def extractInclude(scope : Scope, includeNameData : List[sst.ScopedDatum], includeLocation : SourceLocated)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope), includeLocation)

    val includeExprs = includeResults flatMap { result =>
      // XXX: Should we disallow body defines here in a non-body context?
      // R7RS says (include) should act like a (begin) with the contents of the
      // files. Its example definition of (begin) uses a self-executing lambda
      // which would create a body context. This seems to imply this is allowed.
      apply(result.data)(scope, libraryLoader, result.innerIncludePath)
    }

    et.Begin(includeExprs)
  }

  private def extractSymbolApplication(appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = {
    (appliedSymbol.resolve, operands) match {
      case (SchemePrimitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (SchemePrimitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          extractExpression(falseExpr))
      
      case (SchemePrimitives.If, test :: trueExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          et.Literal(ast.UnspecificValue()))

      case (SchemePrimitives.Set, (scopedSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        scopedSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpression(value))
          case _ =>
            throw new BadSpecialFormException(scopedSymbol, s"Attempted (set!) non-variable ${scopedSymbol.name}") 
        }

      case (SchemePrimitives.Lambda, (restArgDatum : sst.ScopedSymbol) :: definition) =>
        createLambda(List(), Some(restArgDatum), definition)

      case (SchemePrimitives.Lambda, sst.ScopedProperList(fixedArgData) :: definition) =>
        createLambda(fixedArgData, None, definition)

      case (SchemePrimitives.Lambda, sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: definition) =>
        createLambda(fixedArgData, Some(restArgDatum), definition)

      case (SchemePrimitives.SyntaxError, (errorDatum @ sst.NonSymbolLeaf(ast.StringLiteral(errorString))) :: data) =>
        throw new UserDefinedSyntaxError(errorDatum, errorString, data.map(_.unscope))

      case (SchemePrimitives.Include, includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = appliedSymbol match {
          case sst.ScopedSymbol(scope, _) =>
            scope
          case _ =>
            throw new InternalCompilerErrorException("Unable to determine scope of (include)")
        }

        extractInclude(scope, includeNames, appliedSymbol)

      case (NativeFunctionPrimitives.NativeFunction, _) =>
        ExtractNativeFunction(operands, appliedSymbol)

      case (SchemePrimitives.Quasiquote, sst.ScopedProperList(listData) :: Nil) => 
        (new ListQuasiquotationExpander(extractExpression, libraryLoader))(listData)
      
      case (SchemePrimitives.Quasiquote, sst.ScopedVectorLiteral(elements) :: Nil) => 
        (new VectorQuasiquotationExpander(extractExpression, libraryLoader))(elements.toList)
      
      case (SchemePrimitives.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation") 
      
      case (SchemePrimitives.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation") 

      case (syntax : BoundSyntax, operands) =>
        extractExpression(ExpandMacro(syntax, operands, appliedSymbol))

      case (storagLoc : StorageLocation, operands) =>
        et.Apply(et.VarRef(storagLoc), operands.map(extractExpression))

      case otherPrimitive =>
        throw new BadSpecialFormException(appliedSymbol, "Invalid primitive syntax")
    }
  }

  def extractExpression(datum : sst.ScopedDatum)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : et.Expression = (datum match {
    // Normal procedure call
    case sst.ScopedProperList(procedure :: operands) =>
      procedure match {
        case scopedSymbol : sst.ScopedSymbol =>
          // Apply the symbol
          // This is the only way to "apply" syntax and primitives
          // They cannot appear as normal expression values
          extractSymbolApplication(scopedSymbol, operands)

        case _ =>
          // Apply the result of the inner expression
          val procedureExpr = extractExpression(procedure)
          et.Apply(procedureExpr, operands.map(extractExpression))
      }

    case scopedSymbol : sst.ScopedSymbol =>
      scopedSymbol.resolve match {
        case storageLoc : StorageLocation =>
          et.VarRef(storageLoc)

        case syntax : BoundSyntax =>
          throw new MalformedExpressionException(scopedSymbol, "Syntax cannot be used as an expression")

        case primitive : PrimitiveExpression =>
          throw new MalformedExpressionException(scopedSymbol, "Primitive cannot be used as an expression")
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

    case malformed =>
      throw new MalformedExpressionException(malformed, malformed.toString)
  }).assignLocationFrom(datum)

  private def parseSyntaxDefine(datum : sst.ScopedDatum) : ParsedSyntaxDefine = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: (symbol : sst.ScopedSymbol) ::
                         sst.ScopedProperList(
                           sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
                         ) :: Nil) =>
      val literalNames = literals.map { 
        case sst.ScopedSymbol(_, name) => name
        case nonSymbol => throw new BadSpecialFormException(nonSymbol, "Symbol expected in literal list")
      }
      
      val parsedRules = rules map {
        case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
          SyntaxRule(pattern, template)
        case noMatch => throw new BadSpecialFormException(noMatch, "Unable to parse syntax rule")
      }

      ParsedSyntaxDefine(symbol, new BoundSyntax(literalNames, parsedRules))

    case noMatch =>
      throw new BadSpecialFormException(noMatch, "Unrecognized (define-syntax) form")
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

    case sst.ScopedProperList((scopedSymbol : sst.ScopedSymbol) :: operands) =>
      scopedSymbol.resolveOpt match {
        case Some(syntax : BoundSyntax) =>
          // Cheap hack to look for macros early
          // We need to expand them in a body context if they appear in a body
          // Otherwise (define) won't work in macros
          parseDefine(ExpandMacro(syntax, operands, scopedSymbol))

        case Some(InternalPrimitives.DefineReportProcedure) =>
          operands match {
            case (symbol : sst.ScopedSymbol) :: definitionData :: Nil =>
              Some(ParsedVarDefine(symbol, new ReportProcedure(symbol.name), () => {
                extractExpression(definitionData)
              }))

            case _ =>
              throw new BadSpecialFormException(scopedSymbol, "(define-report-procedure) requires exactly two arguments")
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
          (symbol.resolveOpt match {
            case Some(storageLoc : StorageLocation) =>
              // Convert this to a (set!)
              et.MutateVar(storageLoc, exprBlock())

            case Some(_) =>
              throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}") 

            case None  =>
              // This is a fresh binding
              // Place the rest of the body inside an et.Let
              symbol.scope += (symbol.name -> boundValue)
              et.Bind(List(boundValue -> exprBlock()))
          }).assignLocationFrom(datum) :: apply(tailData)

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
