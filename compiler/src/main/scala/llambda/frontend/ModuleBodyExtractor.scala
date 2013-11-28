package llambda.frontend

import llambda._
import llambda.{valuetype => vt}

import collection.mutable.ListBuffer

class ModuleBodyExtractor(libraryLoader : LibraryLoader, includePath : IncludePath) {
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

  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum]) : et.Lambda = {
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
      case ParsedSyntaxDefine(symbol, boundValue) =>
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

  private def extractInclude(scope : Scope, includeNameData : List[sst.ScopedDatum], includeLocation : SourceLocated) : et.Expression = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope), includeLocation)(includePath)

    val includeExprs = includeResults flatMap { result =>
      // XXX: Should we disallow body defines here in a non-body context?
      // R7RS says (include) should act like a (begin) with the contents of the
      // files. Its example definition of (begin) uses a self-executing lambda
      // which would create a body context. This seems to imply this is allowed.
      val includeBodyExtractor = new ModuleBodyExtractor(libraryLoader, result.innerIncludePath)
      includeBodyExtractor(result.data, scope)
    }

    et.Begin(includeExprs)
  }

  private def extractSymbolApplication(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : et.Expression = {
    (boundValue, operands) match {
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

      case (SchemePrimitives.Set, (mutatingSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpression(value))
          case _ =>
            throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) non-variable ${mutatingSymbol.name}") 
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
        val scope = appliedSymbol.scope
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

      case (storagLoc : StorageLocation, operands) =>
        et.Apply(et.VarRef(storagLoc), operands.map(extractExpression))

      case otherPrimitive =>
        throw new BadSpecialFormException(appliedSymbol, "Invalid primitive syntax")
    }
  }

  private def parseDefineDatum(datum : sst.ScopedDatum) : Option[ParsedDefine] = datum match {
    // Could this be define-y?
    case sst.ScopedProperList((appliedSymbol : sst.ScopedSymbol) :: operands) =>
      // Don't do a hard resolve here in case we're referencing something
      // we haven't defined yet
      appliedSymbol.resolveOpt flatMap { boundValue =>
        parseDefine(boundValue, appliedSymbol, operands)
      }

    case _ => None
  }

  private def parseDefine(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : Option[ParsedDefine] =
    (boundValue, operands) match {
      case (SchemePrimitives.Define, (symbol : sst.ScopedSymbol) :: value :: Nil) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          extractExpression(value)
        }))

      case (SchemePrimitives.Define, sst.ScopedProperList((symbol : sst.ScopedSymbol) :: fixedArgs) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, None, body)
        }))
      
      case (SchemePrimitives.Define, sst.ScopedImproperList((symbol : sst.ScopedSymbol) :: fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, Some(restArgDatum), body)
        }))

      case (SchemePrimitives.DefineSyntax, _) =>
        Some(ParseSyntaxDefine(appliedSymbol, operands))
      
      case (SchemePrimitives.DefineRecordType, _) =>
        Some(ParseRecordTypeDefine(appliedSymbol, operands))

      case (InternalPrimitives.DefineReportProcedure, _) =>
        operands match {
          case (symbol : sst.ScopedSymbol) :: definitionData :: Nil =>
            Some(ParsedVarDefine(symbol, new ReportProcedure(symbol.name), () => {
              extractExpression(definitionData)
            }))

          case _ =>
            throw new BadSpecialFormException(appliedSymbol, "(define-report-procedure) requires exactly two arguments")
        }

      case _ => None
  } 

  private def extractApplicationLike(procedure : sst.ScopedDatum, operands : List[sst.ScopedDatum], atOutermostLevel : Boolean) : et.Expression = procedure match {
    case scopedSymbol : sst.ScopedSymbol =>
      scopedSymbol.resolve match {
        case syntax : BoundSyntax =>
          // This is a macro - expand it and call extractGenericExpression again
          extractGenericExpression(ExpandMacro(syntax, operands, scopedSymbol), atOutermostLevel)

        case otherBoundValue =>
          // Try to parse this as a type of definition
          parseDefine(otherBoundValue, scopedSymbol, operands) match {
            case Some(_) if !atOutermostLevel=>
              throw new BadSpecialFormException(scopedSymbol, "Definitions can only be introduced in at the outermost level or at the beginning of a body")

            case Some(ParsedVarDefine(symbol, boundValue, exprBlock)) =>
              // There's a wart in Scheme that allows a top-level (define) to become
              // a (set!) if the value is already defined as a storage location
              return symbol.resolveOpt match {
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
              }

            case Some(ParsedSyntaxDefine(symbol, boundValue)) =>
              // This doesn't create any expression tree nodes 
              symbol.scope += (symbol.name -> boundValue)
              return et.Begin(Nil)

            case Some(ParsedRecordTypeDefine(typeSymbol, recordType, procedures)) =>
              typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

              return et.Bind((procedures.map { case (procedureSymbol, expr) =>
                val storageLoc = new StorageLocation(procedureSymbol.name)

                procedureSymbol.scope += (procedureSymbol.name -> storageLoc)
                (storageLoc, expr) 
              }).toList)

            case None =>
              // Continue below
          }

          // Apply the symbol
          // This is the only way to "apply" syntax and primitives
          // They cannot appear as normal expression values
          extractSymbolApplication(otherBoundValue, scopedSymbol, operands)
        }
        
    case _ =>
      // Apply the result of the inner expression
      val procedureExpr = extractExpression(procedure)
      et.Apply(procedureExpr, operands.map(extractExpression))
  }

  private def extractOutermostLevelExpression(datum : sst.ScopedDatum) : et.Expression =
    extractGenericExpression(datum, true)

  private def extractExpression(datum : sst.ScopedDatum) : et.Expression =
    // Non-body datums must either result in an expression or an exception
    extractGenericExpression(datum, false)
  
  private def extractGenericExpression(datum : sst.ScopedDatum, atOutermostLevel : Boolean) : et.Expression = (datum match {
    case sst.ScopedProperList(procedure :: operands) =>
      // This looks like an application
      extractApplicationLike(procedure, operands, atOutermostLevel)

    case scopedSymbol : sst.ScopedSymbol =>
      scopedSymbol.resolve match {
        case storageLoc : StorageLocation =>
          et.VarRef(storageLoc)

        case syntax : BoundSyntax =>
          throw new MalformedExpressionException(scopedSymbol, "Syntax cannot be used as an expression")

        case primitive : PrimitiveExpression =>
          throw new MalformedExpressionException(scopedSymbol, "Primitive cannot be used as an expression")

        case boundType : BoundType =>
          throw new MalformedExpressionException(scopedSymbol, "Type cannot be used as an expression")
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
  }).map(_.assignLocationFrom(datum))

  def apply(data : List[ast.Datum], evalScope : Scope) : List[et.Expression] = data flatMap { datum => 
    // Annotate our symbols with our current scope
    val scopedDatum = sst.ScopedDatum(evalScope, datum)

    extractOutermostLevelExpression(scopedDatum) match {
      case et.Begin(Nil) => None
      case other => Some(other)
    }
  }
}
