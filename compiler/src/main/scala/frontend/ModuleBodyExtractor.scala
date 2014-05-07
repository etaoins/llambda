package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.frontend.syntax.{ParseSyntaxDefine, ExpandMacro}

import collection.mutable.ListBuffer

class ModuleBodyExtractor(libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) {
  private case class ScopedArgument(symbol : sst.ScopedSymbol, boundValue : StorageLocation)

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

  private def extractBodyDefinition(arguments : List[ScopedArgument], definition : List[sst.ScopedDatum]) : et.Expression = {
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
    } : List[(StorageLocation, et.Expression)]

    // Find the expressions in our body 
    val bodyExprs = bodyData.map(extractExpression) : List[et.Expression]

    // Wrap the bodyExprs in an et.Bind if we're introducing bindings
    // We could easily unconditionally wrap in a possibly empty et.Bind but
    // that makes the unit tests a bit unwieldly
    val boundExprs = bindings match {
      case Nil => bodyExprs
      case _ => et.Bind(bindings) :: bodyExprs
    }

    // Collect our expression list in to a single expression
    et.Expression.fromSequence(boundExprs)
  }

  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum]) : et.Lambda = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time

    // Determine our arguments
    val fixedArgs = fixedArgData.map {
      case symbol : sst.ScopedSymbol => ScopedArgument(symbol, new StorageLocation(symbol.name))
      case datum => throw new BadSpecialFormException(datum, "Symbol expected")
    }

    val restArg = restArgDatum.map { scopedSymbol  =>
      ScopedArgument(scopedSymbol, new StorageLocation(scopedSymbol.name))
    }
    
    val allArgs = fixedArgs ++ restArg.toList

    // Extract the body 
    val bodyExpression = extractBodyDefinition(allArgs, definition)

    et.Lambda(fixedArgs.map(_.boundValue), restArg.map(_.boundValue), bodyExpression)
  }

  private def extractInclude(scope : Scope, includeNameData : List[sst.ScopedDatum], includeLocation : SourceLocated) : et.Expression = {
    val includeResults = ResolveIncludeList(includeNameData.map(_.unscope), includeLocation)(frontendConfig.includePath)

    val includeExprs = includeResults flatMap { result =>
      // XXX: Should we disallow body defines here in a non-body context?
      // R7RS says (include) should act like a (begin) with the contents of the
      // files. Its example definition of (begin) uses a self-executing lambda
      // which would create a body context. This seems to imply this is allowed.
      val innerConfig = frontendConfig.copy(
        includePath=result.innerIncludePath
      )

      val includeBodyExtractor = new ModuleBodyExtractor(libraryLoader, innerConfig)
      includeBodyExtractor(result.data, scope)
    }

    et.Begin(includeExprs)
  }

  private def extractSymbolApplication(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : et.Expression = {
    (boundValue, operands) match {
      case (PrimitiveExpressions.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (PrimitiveExpressions.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          extractExpression(falseExpr))
      
      case (PrimitiveExpressions.If, test :: trueExpr :: Nil) =>
        et.Cond(
          extractExpression(test), 
          extractExpression(trueExpr), 
          et.Literal(ast.UnitValue()))

      case (PrimitiveExpressions.Set, (mutatingSymbol : sst.ScopedSymbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc : StorageLocation =>
            et.MutateVar(storageLoc, extractExpression(value))
          case _ =>
            throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) non-variable ${mutatingSymbol.name}") 
        }

      case (PrimitiveExpressions.Lambda, (restArgDatum : sst.ScopedSymbol) :: definition) =>
        createLambda(List(), Some(restArgDatum), definition)

      case (PrimitiveExpressions.Lambda, sst.ScopedProperList(fixedArgData) :: definition) =>
        createLambda(fixedArgData, None, definition)

      case (PrimitiveExpressions.Lambda, sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: definition) =>
        createLambda(fixedArgData, Some(restArgDatum), definition)

      case (PrimitiveExpressions.SyntaxError, (errorDatum @ sst.NonSymbolLeaf(ast.StringLiteral(errorString))) :: data) =>
        throw new UserDefinedSyntaxError(errorDatum, errorString, data.map(_.unscope))

      case (PrimitiveExpressions.Include, includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = appliedSymbol.scope
        extractInclude(scope, includeNames, appliedSymbol)

      case (PrimitiveExpressions.NativeFunction, _) =>
        ExtractNativeFunction(false, operands, appliedSymbol)
      
      case (PrimitiveExpressions.WorldFunction, _) =>
        ExtractNativeFunction(true, operands, appliedSymbol)

      case (PrimitiveExpressions.Quasiquote, sst.ScopedProperList(listData) :: Nil) => 
        val schemeBase = libraryLoader.loadSchemeBase(frontendConfig)
        (new ListQuasiquotationExpander(extractExpression, schemeBase))(listData)
      
      case (PrimitiveExpressions.Quasiquote, sst.ScopedVectorLiteral(elements) :: Nil) => 
        val schemeBase = libraryLoader.loadSchemeBase(frontendConfig)
        (new VectorQuasiquotationExpander(extractExpression, schemeBase))(elements.toList)
      
      case (PrimitiveExpressions.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation") 
      
      case (PrimitiveExpressions.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation") 

      case (storagLoc : StorageLocation, operands) =>
        et.Apply(et.VarRef(storagLoc), operands.map(extractExpression))

      case (PrimitiveExpressions.AnnotateType, valueExpr :: typeDatum :: Nil) =>
        et.Cast(extractExpression(valueExpr), DatumToValueType(typeDatum))

      case (PrimitiveExpressions.CondExpand, firstClause :: restClauses) =>
        val expandedData = CondExpander(firstClause :: restClauses)(libraryLoader, frontendConfig)

        et.Begin(expandedData.map(extractExpression))

      case (PrimitiveExpressions.Parameterize, sst.ScopedProperList(parameterData) :: bodyData) =>
        val parameters = parameterData map { parameterDatum =>
          parameterDatum match {
            case sst.ScopedProperList(List(parameter, value)) =>
              (extractExpression(parameter), extractExpression(value))

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
      // Don't do a hard resolve here in case we're referencing something
      // we haven't defined yet
      appliedSymbol.resolveOpt flatMap { boundValue =>
        parseDefine(boundValue, appliedSymbol, operands)
      }

    case _ => None
  }

  private def parseDefine(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : Option[ParsedDefine] =
    (boundValue, operands) match {
      case (PrimitiveExpressions.Define, (symbol : sst.ScopedSymbol) :: value :: Nil) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          extractExpression(value)
        }))

      case (PrimitiveExpressions.Define, sst.ScopedProperList((symbol : sst.ScopedSymbol) :: fixedArgs) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, None, body)
        }))
      
      case (PrimitiveExpressions.Define, sst.ScopedImproperList((symbol : sst.ScopedSymbol) :: fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: body) =>
        Some(ParsedVarDefine(symbol, new StorageLocation(symbol.name), () => {
          createLambda(fixedArgs, Some(restArgDatum), body)
        }))

      case (PrimitiveExpressions.DefineSyntax, _) =>
        Some(ParseSyntaxDefine(appliedSymbol, operands))

      case (PrimitiveExpressions.DefineRecordType, _) =>
        Some(ParseRecordTypeDefine(appliedSymbol, operands))

      case (PrimitiveExpressions.DefineType, (typeAlias : sst.ScopedSymbol) :: existingTypeDatum :: Nil) =>
        Some(ParsedSimpleDefine(typeAlias, BoundType(DatumToValueType(existingTypeDatum)))) 

      case (PrimitiveExpressions.DefineReportProcedure, _) =>
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

  private def extractApplicationLike(boundValue : BoundValue, appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum], atOutermostLevel : Boolean) : et.Expression = {
    // Try to parse this as a type of definition
    parseDefine(boundValue, appliedSymbol, operands) match {
      case Some(_) if !atOutermostLevel=>
        throw new DefinitionOutsideTopLevelException(appliedSymbol)

      case Some(ParsedVarDefine(symbol, boundValue, exprBlock)) =>
        // There's a wart in Scheme that allows a top-level (define) to become
        // a (set!) if the value is already defined as a storage location
        symbol.resolveOpt match {
          case Some(storageLoc : StorageLocation) =>
            // Convert this to a (set!)
            et.MutateVar(storageLoc, exprBlock())

          case Some(_) =>
            throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}") 

          case None  =>
            // This is a fresh binding
            // Place the rest of the body inside an et.Bind
            symbol.scope += (symbol.name -> boundValue)
            et.Bind(List(boundValue -> exprBlock()))
        }

      case Some(ParsedSimpleDefine(symbol, boundValue)) =>
        // This doesn't create any expression tree nodes 
        symbol.scope += (symbol.name -> boundValue)
        et.Begin(Nil)

      case Some(ParsedRecordTypeDefine(typeSymbol, recordType, procedures)) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        et.Bind((procedures.map { case (procedureSymbol, expr) =>
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

  private def extractOutermostLevelExpression(datum : sst.ScopedDatum) : et.Expression =
    extractGenericExpression(datum, true)

  private def extractExpression(datum : sst.ScopedDatum) : et.Expression =
    // Non-body datums must either result in an expression or an exception
    extractGenericExpression(datum, false)
  
  private def extractGenericExpression(datum : sst.ScopedDatum, atOutermostLevel : Boolean) : et.Expression = (datum match {
    case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
      appliedSymbol.resolve match {
        case syntax : BoundSyntax =>
          // This is a macro - expand it and call extractGenericExpression again
          extractGenericExpression(ExpandMacro(syntax, cdr, datum), atOutermostLevel)
        
        case otherBoundValue =>
          // Make sure the operands a proper list
          // XXX: Does R7RS only allow macros to be applied as an improper list?
          cdr match {
            case sst.ScopedProperList(operands) =>
              extractApplicationLike(otherBoundValue, appliedSymbol, operands, atOutermostLevel)

            case improperList =>
              throw new MalformedExpressionException(improperList, "Non-syntax cannot be applied as an improper list")
          }
      }

    case sst.ScopedProperList(procedure :: operands) =>
      // Apply the result of the inner expression
      val procedureExpr = extractExpression(procedure)
      et.Apply(procedureExpr, operands.map(extractExpression))

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

    // Additionally treat #!unit as self-evaluating
    case sst.NonSymbolLeaf(literal : ast.UnitValue) =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExpressionException(malformed, malformed.toString)
  }).assignLocationFrom(datum)

  def apply(data : List[ast.Datum], evalScope : Scope) : List[et.Expression] = data flatMap { datum => 
    // Annotate our symbols with our current scope
    val scopedDatum = sst.ScopedDatum(evalScope, datum)

    extractOutermostLevelExpression(scopedDatum) match {
      case et.Begin(Nil) => None
      case other => Some(other)
    }
  }
}
