package llambda

import collection.mutable.ListBuffer

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)

object ExtractBody {
  private def rescope(datum : sst.ScopedDatum, mapping : (Scope, Scope)) : sst.ScopedDatum = { 
    val (from, to) = mapping

    datum match {
      case sst.ScopedPair(car, cdr) =>
        sst.ScopedPair(rescope(car, mapping), rescope(cdr, mapping))
      case sst.ScopedSymbol(scope, name) if scope == from =>
        sst.ScopedSymbol(to, name)
      case other => other
    }
  }

  private def getVar(scope : Scope)(name : String) : BoundValue = {
    scope.get(name).getOrElse {
      throw new UnboundVariableException(name)
    }
  }

  private def defineVar(scope : Scope)(varName : String) : (BoundValue, Scope) = {
    val storageLoc = scope.get(varName) match {
      // Unbound or syntax binding
      case None => new StorageLocation
      case Some(_ : BoundSyntax) => new StorageLocation
      // Reuse existing binding
      case Some(location) => location
    }

    (storageLoc, scope + (varName -> storageLoc))
  }

  private def defineExpression(scope : Scope)(varName : String, varExpr : et.Expression) : (Option[et.Expression], Scope) = {
    val (storageLoc, newScope) = defineVar(scope)(varName)

    val setExpr = et.SetVar(storageLoc, varExpr)

    (Some(setExpr), newScope)
  }
  
  private def createLambda(evalScope : Scope)(fixedArgData : List[sst.ScopedDatum], restArgName : Option[String], body : List[sst.ScopedDatum]) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    val fixedArgNames = fixedArgData.map { datum =>
      datum match {
        case sst.ScopedSymbol(_, name) => name
        case _ => throw new BadSpecialFormException("Symbol expected, found " + datum)
      }
    }

    val fixedArgs : List[ProcedureArg] = fixedArgNames.map { _ =>
      new ProcedureArg
    }

    val restArg : Option[ProcedureArg] = restArgName.map { _ =>
      new ProcedureArg
    }

    // Create a new scope with the args bound to their names
    val binding = collection.mutable.Map[String, BoundValue](
      ((fixedArgNames zip fixedArgs) ++ (restArgName zip restArg).toList) : _*
    )

    val initialScope = new Scope(binding, Some(evalScope))

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

  private def extractApplication(outerScope : Scope)(procedure : BoundValue, operands : List[sst.ScopedDatum]) : et.Expression = {
    (procedure, operands) match {
      case (syntax : BoundSyntax, operands) =>
        extractExpression(ExpandMacro(syntax, operands))

      case (SchemePrimitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (SchemePrimitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Conditional(extractExpression(test), extractExpression(trueExpr), Some(extractExpression(falseExpr)))
      
      case (SchemePrimitives.If, test :: trueExpr :: Nil) =>
        et.Conditional(extractExpression(test), extractExpression(trueExpr), None)

      case (SchemePrimitives.Set, sst.ScopedSymbol(scope, variableName) :: value :: Nil) =>
        et.SetVar(getVar(scope)(variableName), extractExpression(value))

      case (SchemePrimitives.Lambda, sst.ScopedSymbol(_, restArg) :: body) =>
        createLambda(outerScope)(List(), Some(restArg), body)

      case (SchemePrimitives.Lambda, sst.ScopedProperList(fixedArgData) :: body) =>
        createLambda(outerScope)(fixedArgData, None, body)

      case (SchemePrimitives.Lambda, sst.ScopedImproperList(fixedArgData, sst.ScopedSymbol(_, restArg)) :: body) =>
        createLambda(outerScope)(fixedArgData, Some(restArg), body)

      case _ =>
        et.ProcedureCall(procedure, operands.map(extractExpression(_)))
    }
  }

  def extractExpression(datum : sst.ScopedDatum) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(sst.ScopedSymbol(scope, procedureName) :: operands) =>
      extractApplication(scope)(getVar(scope)(procedureName), operands)

    case sst.ScopedSymbol(scope, variableName) =>
      et.VarReference(getVar(scope)(variableName))

    // These all evaluate to themselves. See R7RS section 4.1.2
    case sst.NonSymbolAtom(literal : ast.NumberLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolAtom(literal : ast.StringLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolAtom(literal : ast.CharLiteral) =>
      et.Literal(literal)
    case sst.NonSymbolAtom(literal : ast.Vector) =>
      et.Literal(literal)
    case sst.NonSymbolAtom(literal : ast.ByteVector) =>
      et.Literal(literal)
    case sst.NonSymbolAtom(literal : ast.BooleanLiteral) =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExpressionException(malformed.toString)
  }

  private def defineSyntax(evalScope : Scope)(datum : sst.ScopedDatum) : Scope = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: sst.ScopedSymbol(assignScope, keyword) ::
                         sst.ScopedProperList(
                           sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
                         ) :: Nil) =>
      
      // Create our new scope before and 
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
      defineExpression(assignScope)(varName, extractExpression(value))

    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedProperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs) :: body) =>
      defineExpression(assignScope)(varName, createLambda(evalScope)(fixedArgs, None, body))
    
    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedImproperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs, sst.ScopedSymbol(_, restArg)) :: body) =>
      defineExpression(assignScope)(varName, createLambda(evalScope)(fixedArgs, Some(restArg), body))

    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: _) =>
      (None, defineSyntax(evalScope)(datum))
    
    case expressionDatum =>
      // Scope is unmodified
      (Some(extractExpression(expressionDatum)), evalScope)
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
