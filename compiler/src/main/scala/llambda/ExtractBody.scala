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
      case sst.ScopedSymbol(scope, name) if scope == from =>
        sst.ScopedSymbol(to, name)
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
    val storageLoc = scope.get(varName) match {
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
  
  private def createLambda(evalScope : Scope)(selfRef : Option[(String, BoundValue)], fixedArgData : List[sst.ScopedDatum], restArgName : Option[String], body : List[sst.ScopedDatum]) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    val fixedArgNames = fixedArgData.map { datum =>
      datum match {
        case sst.ScopedSymbol(_, name) => name
        case _ => throw new BadSpecialFormException("Symbol expected, found " + datum)
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
        createLambda(outerScope)(None, List(), Some(restArg), body)

      case (SchemePrimitives.Lambda, sst.ScopedProperList(fixedArgData) :: body) =>
        createLambda(outerScope)(None, fixedArgData, None, body)

      case (SchemePrimitives.Lambda, sst.ScopedImproperList(fixedArgData, sst.ScopedSymbol(_, restArg)) :: body) =>
        createLambda(outerScope)(None, fixedArgData, Some(restArg), body)

      case (SchemePrimitives.SyntaxError, sst.NonSymbolLeaf(ast.StringLiteral(errorString)) :: data) =>
        throw new UserDefinedSyntaxError(errorString, data.map(_.unscope))

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
        extractExpression(value)
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
        case _ =>
          (Some(extractExpression(datum)), evalScope)
      }
    
    case _ =>
      // Scope is unmodified
      (Some(extractExpression(datum)), evalScope)
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
