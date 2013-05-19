package llambda

import collection.mutable.ListBuffer

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)
class UserDefinedSyntaxError(errorString  : String, data : List[ast.Datum]) extends SemanticException(errorString + " " + data.map(_.toString).mkString(" "))

object ExtractBody {
  private def rescope(datum : sst.ScopedDatum, mapping : (Scope, Scope)) : sst.ScopedDatum = { 
    val (from, to) = mapping

    datum match {
      case sst.ScopedPair(car, cdr) =>
        sst.ScopedPair(rescope(car, mapping), rescope(cdr, mapping))
      case sst.ScopedSymbol(scope, name) =>
        if (scope == from) {
          sst.ScopedSymbol(to, name)
        }
        else {
          datum
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

  private def defineVar(scope : Scope)(varName : String) : (BoundValue, Scope) = {
    val storageLoc = scope.bindings.get(varName) match {
      // Unbound or syntax binding
      case None => new StorageLocation
      case Some(_ : BoundSyntax) => new StorageLocation
      // Reuse existing binding
      case Some(location) => location
    }

    (storageLoc, scope + (varName -> storageLoc))
  }

  private def defineExpression(scope : Scope, varName : String)(exprBlock : BoundValue => et.Expression) : (Option[et.Expression], Scope) = {
    val (storageLoc, newScope) = defineVar(scope)(varName)

    val setExpr = et.SetVar(storageLoc, exprBlock(storageLoc))

    (Some(setExpr), newScope)
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
  
  private def createLambda(evalScope : Scope)(selfRef : Option[(String, BoundValue)], fixedArgData : List[sst.ScopedDatum], restArgName : Option[String], body : List[sst.ScopedDatum]) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    val fixedArgNames = fixedArgData.map { datum =>
      datum match {
        case sst.ScopedSymbol(_, name) => name
        case _ => throw new BadSpecialFormException("Symbol expected, found " + datum.unscope)
      }
    }

    if (fixedArgNames.toSet.size < fixedArgNames.size) {
      throw new BadSpecialFormException("Duplicate formal parameters: " + fixedArgNames)
    }

    val fixedArgs : List[ProcedureArg] = fixedArgNames.map { _ =>
      new ProcedureArg
    }

    val restArg : Option[ProcedureArg] = restArgName.map { _ =>
      new ProcedureArg
    }

    // Create a new scope with the args bound to their names
    val bindingsList = (fixedArgNames zip fixedArgs) ++  (restArgName zip restArg).toList ++ selfRef.toList
    val binding = collection.mutable.Map[String, BoundValue](bindingsList : _*)
    val initialScope = new Scope(binding, Some(evalScope))

    // Parse our body
    val expressions = ListBuffer[et.Expression]()

    body.foldLeft(initialScope) { (scope, datum) =>
      // Replace instances of our defining scope with the inner scope
      val rescopedDatum = rescope(datum, evalScope -> scope)
    
      val (expr, newScope) = extractBodyExpression(scope)(rescopedDatum)
      expressions ++= expr.toList

      newScope
    }

    et.Procedure(fixedArgs, restArg, expressions.toList)
  }

  private def extractApplication(evalScope : Scope)(procedure : et.Expression, operands : List[sst.ScopedDatum]) : et.Expression = {
    (procedure, operands) match {
      case (et.VarReference(syntax : BoundSyntax), operands) =>
        extractExpression(evalScope)(ExpandMacro(syntax, operands))

      case (et.VarReference(SchemePrimitives.Quote), innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (et.VarReference(SchemePrimitives.If), test :: trueExpr :: falseExpr :: Nil) =>
        et.Conditional(
          extractExpression(evalScope)(test), 
          extractExpression(evalScope)(trueExpr), 
          Some(extractExpression(evalScope)(falseExpr)))
      
      case (et.VarReference(SchemePrimitives.If), test :: trueExpr :: Nil) =>
        et.Conditional(
          extractExpression(evalScope)(test), 
          extractExpression(evalScope)(trueExpr), 
          None)

      case (et.VarReference(SchemePrimitives.Set), sst.ScopedSymbol(scope, variableName) :: value :: Nil) =>
        et.SetVar(getVar(scope)(variableName), extractExpression(evalScope)(value))

      case (et.VarReference(SchemePrimitives.Lambda), sst.ScopedSymbol(_, restArg) :: body) =>
        createLambda(evalScope)(None, List(), Some(restArg), body)

      case (et.VarReference(SchemePrimitives.Lambda), sst.ScopedProperList(fixedArgData) :: body) =>
        createLambda(evalScope)(None, fixedArgData, None, body)

      case (et.VarReference(SchemePrimitives.Lambda), sst.ScopedImproperList(fixedArgData, sst.ScopedSymbol(_, restArg)) :: body) =>
        createLambda(evalScope)(None, fixedArgData, Some(restArg), body)

      case (et.VarReference(SchemePrimitives.SyntaxError), sst.NonSymbolLeaf(ast.StringLiteral(errorString)) :: data) =>
        throw new UserDefinedSyntaxError(errorString, data.map(_.unscope))

      case (et.VarReference(NativeFunctionPrimitives.NativeFunction), _) =>
        extractNativeFunction(operands)

      case _ =>
        et.ProcedureCall(procedure, operands.map(extractExpression(evalScope)(_)))
    }
  }

  def extractExpression(evalScope : Scope)(datum : sst.ScopedDatum) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(procedure :: operands) =>
      val procedureExpr = extractExpression(evalScope)(procedure)
      extractApplication(evalScope)(procedureExpr, operands)

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

  private def defineSyntax(evalScope : Scope)(datum : sst.ScopedDatum) : Scope = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: sst.ScopedSymbol(assignScope, keyword) ::
                         sst.ScopedProperList(
                           sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
                         ) :: Nil) =>
      
      // Create our new scope before 
      val newScope = new Scope(collection.mutable.Map(), Some(evalScope))

      val literalNames = literals.map { 
        case sst.ScopedSymbol(_, name) => name
        case nonSymbol => throw new BadSpecialFormException("Symbol expected in literal list, found " + nonSymbol)
      }

      // Rescope the rules to our new scope
      val rescopedRules = rules map { rule =>
        rescope(rule, evalScope -> newScope)
      }
        
      val parsedRules = rescopedRules map {
        case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
          SyntaxRule(pattern, template)
        case noMatch => throw new BadSpecialFormException("Unable to parse syntax rule " + noMatch)
      }

      // Inject the binding in to the new scope to allow recursive macro expansion
      newScope += keyword -> BoundSyntax(literalNames, parsedRules)

      newScope
    case noMatch =>
      throw new BadSpecialFormException("Unrecognized define-syntax form " + noMatch)
  }

  private def extractBodyExpression(evalScope : Scope)(datum : sst.ScopedDatum) : (Option[et.Expression], Scope) = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define") :: sst.ScopedSymbol(assignScope, varName) :: value :: Nil) =>
      defineExpression(assignScope, varName) { _ => 
        extractExpression(evalScope)(value)
      }

    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedProperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs) :: body) =>
      defineExpression(assignScope, varName) { selfRef => 
        createLambda(evalScope)(Some(varName -> selfRef), fixedArgs, None, body)
      }
    
    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedImproperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs, sst.ScopedSymbol(_, restArg)) :: body) =>
      defineExpression(assignScope, varName) { selfRef =>
        createLambda(evalScope)(Some(varName -> selfRef), fixedArgs, Some(restArg), body)
      }

    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: _) =>
      (None, defineSyntax(evalScope)(datum))

    // Cheap hack to look for macros early
    // We need to expand them in a body context if they appear in a body
    // Otherwise (define) etc won't work in macros
    case sst.ScopedProperList(sst.ScopedSymbol(scope, procedureName) :: operands) =>
      scope.get(procedureName) match {
        case Some(syntax : BoundSyntax) =>
          extractBodyExpression(evalScope)(ExpandMacro(syntax, operands))

        case Some(InternalPrimitives.DefineReportProcedure) =>
          operands match {
            case sst.ScopedSymbol(_, varName) :: definitionData :: Nil =>
              val reportProc = new ReportProcedure(varName)
              val newScope = scope + (varName -> reportProc)
              val definitionExpr = extractExpression(evalScope)(definitionData)

              (Some(et.SetVar(reportProc, definitionExpr)), newScope)

            case _ =>
              throw new BadSpecialFormException("define-report-procedure requires exactly two arguments")
          }

        case _ =>
          (Some(extractExpression(evalScope)(datum)), evalScope)
      }

    case _ =>
      // Scope is unmodified
      (Some(extractExpression(evalScope)(datum)), evalScope)
  }

  def apply(data : List[ast.Datum])(implicit initialScope : Scope) : (List[et.Expression], Scope) = {
    val expressions = ListBuffer[et.Expression]()

    val finalScope = data.foldLeft(initialScope) { (evalScope, datum) =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      val (expr, newScope) = extractBodyExpression(evalScope)(scopedDatum)
      expressions ++= expr.toList

      newScope
    }

    (expressions.toList, finalScope)
  }
}
