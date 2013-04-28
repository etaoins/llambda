package llambda

import collection.mutable.ListBuffer

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)

object ExtractBody {
  private def rescope(datum : sst.ScopedDatum, mapping : (Scope, Scope)) : sst.ScopedDatum = { 
    val (from, to) = mapping

    val newScope = if (datum.scope == from) {
      // Map to our new scope
      to
    }
    else {
      // Keep the existing scope
      datum.scope
    }

    datum match {
      case sst.ScopedPair(_, car, cdr) =>
        sst.ScopedPair(newScope, rescope(car, mapping), rescope(cdr, mapping))
      case sst.ScopedAtom(_, atom) =>
        sst.ScopedAtom(newScope, atom)
    }
  }

  private def getVar(name : String)(scope : Scope) : BoundValue = {
    scope.get(name).getOrElse {
      throw new UnboundVariableException(name)
    }
  }

  private def defineVar(varName : String)(scope : Scope) : (BoundValue, Scope) = {
    val storageLoc = scope.get(varName) match {
      // Unbound or syntax binding
      case None => new StorageLocation
      case Some(_ : BoundSyntax) => new StorageLocation
      // Reuse existing binding
      case Some(location) => location
    }

    (storageLoc, scope + (varName -> storageLoc))
  }

  private def defineExpression(varName : String, varExpr : et.Expression)(scope : Scope) : (Option[et.Expression], Scope) = {
    val (storageLoc, newScope) = defineVar(varName)(scope)

    // Use the old scope when expanding the definitions
    val setExpr = et.SetVar(storageLoc, varExpr)

    (Some(setExpr), newScope)
  }
  
  private def createLambda(fixedArgData : List[sst.ScopedDatum], restArgName : Option[String], body : List[sst.ScopedDatum])(definingScope : Scope) : et.Procedure = {
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
    val binding : Map[String, ProcedureArg] =
      ((fixedArgNames zip fixedArgs) ++ (restArgName zip restArg).toList).toMap

    val initialScope = new Scope(binding, Some(definingScope))

    val expressions = ListBuffer[et.Expression]()

    body.foldLeft(initialScope) { (scope, datum) =>
      // Replace instances of our defining scope with the inner scope
      val rescopedDatum = rescope(datum, definingScope -> scope)
    
      val (expr, newScope) = extractBodyExpression(rescopedDatum)
      expressions ++= expr.toList

      newScope
    }

    et.Procedure(fixedArgs, restArg, expressions.toList)
  }

  private def extractApplication(procedure : BoundValue, operands : List[sst.ScopedDatum])(outerScope : Scope) : et.Expression = {
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
        et.SetVar(getVar(variableName)(scope), extractExpression(value))

      case (SchemePrimitives.Lambda, sst.ScopedSymbol(_, restArg) :: body) =>
        createLambda(List(), Some(restArg), body)(outerScope)

      case (SchemePrimitives.Lambda, sst.ScopedProperList(fixedArgData) :: body) =>
        createLambda(fixedArgData, None, body)(outerScope)

        /*
      case (SchemePrimitives.Lambda, ast.ImproperList(fixedArgData, ast.Symbol(restArg)) :: body) =>
        createLambda(fixedArgData, Some(restArg), body)(scope)*/

      case _ =>
        et.ProcedureCall(procedure, operands.map(extractExpression(_)))
    }
  }

  def extractExpression(datum : sst.ScopedDatum) : et.Expression = datum match {
    // Normal procedure call
    case sst.ScopedProperList(sst.ScopedSymbol(scope, procedureName) :: operands) =>
      extractApplication(getVar(procedureName)(scope), operands)(scope)

    case sst.ScopedSymbol(scope, variableName) =>
      et.VarReference(getVar(variableName)(scope))

    // These all evaluate to themselves. See R7RS section 4.1.2
    case sst.ScopedAtom(_, literal : ast.NumberLiteral) =>
      et.Literal(literal)
    case sst.ScopedAtom(_, literal : ast.StringLiteral) =>
      et.Literal(literal)
    case sst.ScopedAtom(_, literal : ast.CharLiteral) =>
      et.Literal(literal)
    case sst.ScopedAtom(_, literal : ast.Vector) =>
      et.Literal(literal)
    case sst.ScopedAtom(_, literal : ast.ByteVector) =>
      et.Literal(literal)
    case sst.ScopedAtom(_, literal : ast.BooleanLiteral) =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExpressionException(malformed.toString)
  }

  private def defineSyntax(datum : sst.ScopedDatum) : Scope = datum match {
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

      datum.scope + (keyword ->  BoundSyntax(literalNames, parsedRules))
    case noMatch =>
      throw new BadSpecialFormException("Unrecognized define-syntax form " + noMatch)
  }

  private def extractBodyExpression(datum : sst.ScopedDatum) : (Option[et.Expression], Scope) = datum match {
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define") :: sst.ScopedSymbol(assignScope, varName) :: value :: Nil) =>
      defineExpression(varName, extractExpression(value))(assignScope)

    case sst.ScopedProperList(sst.ScopedSymbol(defineScope, "define") :: sst.ScopedProperList(sst.ScopedSymbol(assignScope, varName) :: fixedArgs) :: body) =>
      defineExpression(varName, createLambda(fixedArgs, None, body)(defineScope))(assignScope)
    
/*
    case sst.ScopedProperList(sst.ScopedSymbol(_, "define") :: sst.ScopedImproperList(ast.Symbol(varName) :: fixedArgs, ast.Symbol(restArg)) :: body) =>
      defineExpression(varName, createLambda(fixedArgs, Some(restArg), body))*/

    case sst.ScopedProperList(sst.ScopedSymbol(_, "define-syntax") :: _) =>
      (None, defineSyntax(datum))
    
    case expressionDatum =>
      // Scope is unmodified
      (Some(extractExpression(expressionDatum)), datum.scope)
  }

  def apply(data : List[ast.Datum])(implicit initialScope : Scope) : (List[et.Expression], Scope) = {
    val expressions = ListBuffer[et.Expression]()

    val finalScope = data.foldLeft(initialScope) { (scope, datum) =>
      // Annotate our AST with our initial scope
      val scopedDatum = sst.ScopedDatum(scope, datum)

      val (expr, newScope) = extractBodyExpression(scopedDatum)
      expressions ++= expr.toList

      newScope
    }

    (expressions.toList, finalScope)
  }
}
