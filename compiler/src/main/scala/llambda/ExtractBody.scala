package llambda

import collection.mutable.ListBuffer

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)
class UserDefinedSyntaxError(errorString  : String, data : List[ast.Datum]) extends SemanticException(errorString + " " + data.map(_.toString).mkString(" "))

object ExtractBody {
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

  private def defineVar(scope : Scope)(varName : String) : BoundValue = {
    val storageLoc = scope.bindings.get(varName) match {
      // Unbound or syntax binding
      case None => new StorageLocation
      case Some(_ : BoundSyntax) => new StorageLocation
      // Reuse existing binding
      case Some(location) => location
    }

    scope += (varName -> storageLoc)
    storageLoc
  }

  private def defineExpression(scope : Scope, varName : String)(exprBlock : => et.Expression) : et.Expression = {
    val storageLoc =  defineVar(scope)(varName)
    et.SetVar(storageLoc, exprBlock)
  }

  private def createNativeFunction(fixedArgData : List[sst.ScopedDatum], restArgType : Option[String], returnTypeString : String, nativeSymbol : String) : et.NativeFunction = {
    def parseNativeType(typeString : String) : nfi.NativeType = typeString match {
      case "bool"   => nfi.Bool
      case "int8"   => nfi.Int8
      case "int16"  => nfi.Int16
      case "int32"  => nfi.Int32
      case "int64"  => nfi.Int64
      case "float"  => nfi.Float
      case "double" => nfi.Double

      // XXX: This assumes Unix-like LP64: 64bit Linux, FreeBSD, Mac OS X, etc 
      // These aliases are here so we can do the right thing when porting to other archs
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
  
  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgDatum : Option[sst.ScopedSymbol], body : List[sst.ScopedDatum]) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    case class ScopedArgument(scope : Scope, name : String, boundValue : ProcedureArg)

    // Determine our arguments
    val fixedArgs = fixedArgData.map {
      case sst.ScopedSymbol(scope, name) => ScopedArgument(scope, name, new ProcedureArg)
      case datum => throw new BadSpecialFormException("Symbol expected, found " + datum.unscope)
    }

    val restArg = restArgDatum.map { scopedSymbol  =>
      ScopedArgument(scopedSymbol.scope, scopedSymbol.name, new ProcedureArg)
    }
    
    val allArgs = fixedArgs ++ restArg.toList

    // Find are all the scopes in the body
    val bodyScopes = body.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ uniqueScopes(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = allArgs groupBy(_.scope)
      
    val scopeMapping = (bodyScopes map { outerScope => 
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

    // Find all scopes that don't have an inner scope created due to lack of args
    // Otherwise argless lambdas will modify their parent scope
    
    // Parse our body
    val expressions = body flatMap { datum =>
      // Replace instances of our defining scope with the inner scope
      val rescopedDatum = rescope(datum, scopeMapping)
      extractBodyExpression(rescopedDatum)
    }

    et.Procedure(fixedArgs.map(_.boundValue), restArg.map(_.boundValue), expressions.toList)
  }

  private def extractApplication(procedure : et.Expression, operands : List[sst.ScopedDatum]) : et.Expression = {
    (procedure, operands) match {
      case (et.VarReference(syntax : BoundSyntax), operands) =>
        extractExpression(ExpandMacro(syntax, operands))

      case (et.VarReference(SchemePrimitives.Quote), innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (et.VarReference(SchemePrimitives.If), test :: trueExpr :: falseExpr :: Nil) =>
        et.Conditional(
          extractExpression(test), 
          extractExpression(trueExpr), 
          extractExpression(falseExpr))
      
      case (et.VarReference(SchemePrimitives.If), test :: trueExpr :: Nil) =>
        et.Conditional(
          extractExpression(test), 
          extractExpression(trueExpr), 
          et.Literal(ast.UnspecificValue))

      case (et.VarReference(SchemePrimitives.Set), sst.ScopedSymbol(scope, variableName) :: value :: Nil) =>
        et.SetVar(getVar(scope)(variableName), extractExpression(value))

      case (et.VarReference(SchemePrimitives.Lambda), (restArgDatum : sst.ScopedSymbol) :: body) =>
        createLambda(List(), Some(restArgDatum), body)

      case (et.VarReference(SchemePrimitives.Lambda), sst.ScopedProperList(fixedArgData) :: body) =>
        createLambda(fixedArgData, None, body)

      case (et.VarReference(SchemePrimitives.Lambda), sst.ScopedImproperList(fixedArgData, (restArgDatum : sst.ScopedSymbol)) :: body) =>
        createLambda(fixedArgData, Some(restArgDatum), body)

      case (et.VarReference(SchemePrimitives.SyntaxError), sst.NonSymbolLeaf(ast.StringLiteral(errorString)) :: data) =>
        throw new UserDefinedSyntaxError(errorString, data.map(_.unscope))

      case (et.VarReference(NativeFunctionPrimitives.NativeFunction), _) =>
        extractNativeFunction(operands)

      case _ =>
        et.ProcedureCall(procedure, operands.map(extractExpression))
    }
  }

  def extractExpression(datum : sst.ScopedDatum) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(procedure :: operands) =>
      val procedureExpr = extractExpression(procedure)
      extractApplication(procedureExpr, operands)

    case sst.ScopedSymbol(scope, variableName) =>
      et.VarReference(getVar(scope)(variableName))

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

  private def defineSyntax(datum : sst.ScopedDatum) : Unit = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: sst.ScopedSymbol(assignScope, keyword) ::
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

      // Inject the binding in to the new scope to allow recursive macro expansion
      assignScope += keyword -> BoundSyntax(literalNames, parsedRules)

    case noMatch =>
      throw new BadSpecialFormException("Unrecognized define-syntax form " + noMatch)
  }

  private def extractBodyExpression(datum : sst.ScopedDatum) : Option[et.Expression] = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define") :: sst.ScopedSymbol(assignScope, varName) :: value :: Nil) =>
      Some(defineExpression(assignScope, varName) { 
        extractExpression(value)
      })

    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedProperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs) :: body) =>
      Some(defineExpression(assignScope, varName) {
        createLambda(fixedArgs, None, body)
      })
    
    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedImproperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs, (restArgDatum : sst.ScopedSymbol)) :: body) =>
      Some(defineExpression(assignScope, varName) {
        createLambda(fixedArgs, Some(restArgDatum), body)
      })

    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: _) =>
      defineSyntax(datum)
      None

    // Cheap hack to look for macros early
    // We need to expand them in a body context if they appear in a body
    // Otherwise (define) etc won't work in macros
    case sst.ScopedProperList(sst.ScopedSymbol(scope, procedureName) :: operands) =>
      scope.get(procedureName) match {
        case Some(syntax : BoundSyntax) =>
          extractBodyExpression(ExpandMacro(syntax, operands))

        case Some(InternalPrimitives.DefineReportProcedure) =>
          operands match {
            case sst.ScopedSymbol(_, varName) :: definitionData :: Nil =>
              val reportProc = new ReportProcedure(varName)
              scope += (varName -> reportProc)
              val definitionExpr = extractExpression(definitionData)

              Some(et.SetVar(reportProc, definitionExpr))

            case _ =>
              throw new BadSpecialFormException("define-report-procedure requires exactly two arguments")
          }

        case _ =>
          Some(extractExpression(datum))
      }

    case _ =>
      // Scope is unmodified
      Some(extractExpression(datum))
  }

  def apply(data : List[ast.Datum])(implicit evalScope : Scope) : List[et.Expression] = {
    val expressions = ListBuffer[et.Expression]()

    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)
      extractBodyExpression(scopedDatum)
    }
  }
}
