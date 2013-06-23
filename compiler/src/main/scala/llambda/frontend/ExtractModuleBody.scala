package llambda.frontend

import llambda._

import collection.mutable.ListBuffer

object ExtractModuleBody {
  // If expr is None it doesn't need to exist in the expression tree; it's a 
  // frontend-only binding such as a macro
  abstract sealed class ParsedDefine {
    val name : sst.ScopedSymbol
    val value : BoundValue
  }

  private case class ParsedVarDefine(name : sst.ScopedSymbol, value : BoundValue, expr : () => et.Expression) extends ParsedDefine
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

  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgType : Option[String], returnTypeString : String, nativeSymbol : String) : et.NativeFunction = {
    def parseNativeType(typeString : String) : nfi.NativeType = typeString match {
      case "bool32" => nfi.Bool32
      case "int8"   => nfi.Int8
      case "int16"  => nfi.Int16
      case "int32"  => nfi.Int32
      case "int64"  => nfi.Int64
      case "float"  => nfi.Float
      case "double" => nfi.Double

      // XXX: This assumes Unix-like LP64: 64bit Linux, FreeBSD, Mac OS X, etc 
      // These aliases are here so we can do the right thing when porting to other archs
      case "bool"  => nfi.Bool32
      case "short" => nfi.Int16
      case "int"   => nfi.Int32
      case "long"  => nfi.Int64
      case "wchar" => nfi.Int32

      case "boxeddatum" => nfi.BoxedDatum
      case _ => throw new BadSpecialFormException("Unknown native type: " + typeString)
    }
    
    var fixedArgTypes = fixedArgData map {
      case sst.ScopedSymbol(_, typeName) => parseNativeType(typeName)
      case nonsymbol => throw new BadSpecialFormException("Excepted native type name to be string: " + nonsymbol)
    }

    val hasRestArg = restArgType match {
      case Some("boxeddatum") => true
      case Some(other) => throw new BadSpecialFormException("Only boxeddatum can be used as a rest argument. Found: " + other)
      case None => false
    }

    val returnType = returnTypeString match {
      case "void" => None
      case _ => Some(parseNativeType(returnTypeString))
    }

    et.NativeFunction(
      fixedArgs = fixedArgTypes,
      hasRestArg = hasRestArg,
      returnType = returnType,
      nativeSymbol = nativeSymbol)
  }

  private def extractNativeFunction(operands : List[sst.ScopedDatum]) : et.NativeFunction = operands match {
    // These mirror the lambda forms
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedProperList(fixedArgs) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(fixedArgs, None, returnType, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedSymbol(_, restArgType) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(Nil, Some(restArgType), returnType, nativeSymbol)
    
    case sst.NonSymbolLeaf(ast.StringLiteral(nativeSymbol)) :: sst.ScopedImproperList(fixedArgs, sst.ScopedSymbol(_, restArgType)) :: sst.ScopedSymbol(_, returnType) :: Nil =>
      createNativeFunction(fixedArgs, Some(restArgType), returnType, nativeSymbol)

    case _ =>
      throw new BadSpecialFormException("Bad native-function operands: " + operands.mkString(" "))
  }
  
  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], definition : List[sst.ScopedDatum]) : et.Lambda = {
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
    } : List[(BoundValue, et.Expression)]

    // Find the expressions in our body 
    val bodyExprs = bodyData.map(extractExpression) : List[et.Expression]

    // Wrap the bodyExprs in an et.Let if we're introducing bindings
    // We could easily unconditionally wrap in a possibly empty et.Let but
    // that makes the unit tests a bit unwieldly
    val boundExprs = bindings match {
      case Nil => bodyExprs
      case _ => List(et.Let(bindings, bodyExprs))
    }
      
    et.Lambda(fixedArgs.map(_.boundValue), restArg.map(_.boundValue), boundExprs)
  }

  private def extractApplication(procedure : et.Expression, operands : List[sst.ScopedDatum]) : et.Expression = {
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
        et.MutateVar(getVar(scope)(variableName), extractExpression(value))

      case (et.VarRef(SchemePrimitives.Lambda), (restArgDatum : sst.ScopedSymbol) :: definition) =>
        createLambda(List(), Some(restArgDatum), definition)

      case (et.VarRef(SchemePrimitives.Lambda), sst.ScopedProperList(fixedArgData) :: definition) =>
        createLambda(fixedArgData, None, definition)

      case (et.VarRef(SchemePrimitives.Lambda), sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: definition) =>
        createLambda(fixedArgData, Some(restArgDatum), definition)

      case (et.VarRef(SchemePrimitives.SyntaxError), sst.NonSymbolLeaf(ast.StringLiteral(errorString)) :: data) =>
        throw new UserDefinedSyntaxError(errorString, data.map(_.unscope))

      case (et.VarRef(NativeFunctionPrimitives.NativeFunction), _) =>
        extractNativeFunction(operands)

      case _ =>
        et.Apply(procedure, operands.map(extractExpression))
    }
  }

  def extractExpression(datum : sst.ScopedDatum) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(procedure :: operands) =>
      val procedureExpr = extractExpression(procedure)
      extractApplication(procedureExpr, operands)

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
    case sst.NonSymbolLeaf(literal : ast.ByteVector) =>
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

  private def parseDefine(datum : sst.ScopedDatum) : Option[ParsedDefine] = datum match {
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
          // Otherwise (define) etc won't work in macros
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

  def apply(data : List[ast.Datum])(implicit evalScope : Scope) : List[et.Expression] = data match {
    case Nil => Nil
    case datum :: tailData =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      // Try to parse this as a type of definition
      parseDefine(scopedDatum) match {
        case Some(ParsedVarDefine(symbol, boundValue, exprBlock)) =>
          // There's a wart in Scheme that allows a top-level (define) to become
          // a (set!) if the value is already defined as a storage location
          symbol.scope.get(symbol.name) match {
            case Some(storageLoc : StorageLocation) =>
              // Convert this to a (set!)
              et.MutateVar(storageLoc, exprBlock()) :: apply(tailData)
            case _  =>
              // This is a fresh binding
              // Place the rest of the body inside an et.Let
              symbol.scope += (symbol.name -> boundValue)
              et.Let(List(boundValue -> exprBlock()), apply(tailData)) :: Nil
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
