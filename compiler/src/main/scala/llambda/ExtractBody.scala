package llambda

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)

object ExtractBody {
  private def getVar(name : String)(implicit scope : Scope) : BoundValue = {
    scope.get(name).getOrElse {
      throw new UnboundVariableException(name)
    }
  }

  private def defineVar(varName : String)(scope : Scope) : (BoundValue, Scope) = {
    val storageLoc = scope.get(varName) match {
      // Unbound or syntax binding
      case None => new StorageLocation
      case Some(_ : SyntaxBinding) => new StorageLocation
      // Reuse existing binding
      case Some(location) => location
    }

    (storageLoc, scope + (varName -> storageLoc))
  }

  private def continueBodyWithDefine(restData : List[ast.Datum], varName : String, varExpr : et.Expression)(implicit scope : Scope) : (List[et.Expression], Scope) = {
    val (storageLoc, newScope) = defineVar(varName)(scope)

    // Use the old scope when expanding the definitions
    val setExpr = et.SetVar(storageLoc, varExpr)
    
    ExtractBody(restData)(newScope) match {
      case (exprs, finalScope) =>
        (setExpr :: exprs, finalScope)
    }
  }
  
  private def createLambda(fixedArgData : List[ast.Datum], restArgName : Option[String], body : List[ast.Datum])(implicit parentScope : Scope) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    val fixedArgNames = fixedArgData.map { datum =>
      datum match {
        case ast.Symbol(name) => name
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

    val initialScope = new Scope(binding, Some(parentScope))

    // Parse the body - we can discard the scope after
    val (expressions, _) = ExtractBody(body)(initialScope)

    et.Procedure(fixedArgs, restArg, expressions)
  }

  private def extractApplication(procedure : BoundValue, operands : List[ast.Datum])(implicit scope : Scope) : et.Expression = {
    (procedure, operands) match {
      case (SchemePrimitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum)

      case (SchemePrimitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Conditional(extractExpression(test), extractExpression(trueExpr), Some(extractExpression(falseExpr)))
      
      case (SchemePrimitives.If, test :: trueExpr :: Nil) =>
        et.Conditional(extractExpression(test), extractExpression(trueExpr), None)

      case (SchemePrimitives.Set, ast.Symbol(variableName) :: value :: Nil) =>
        et.SetVar(getVar(variableName), extractExpression(value))

      case (SchemePrimitives.Lambda, ast.Symbol(restArg) :: body) =>
        createLambda(List(), Some(restArg), body)(scope)

      case (SchemePrimitives.Lambda, ast.ProperList(fixedArgData) :: body) =>
        createLambda(fixedArgData, None, body)

      case (SchemePrimitives.Lambda, ast.ImproperList(fixedArgData, ast.Symbol(restArg)) :: body) =>
        createLambda(fixedArgData, Some(restArg), body)(scope)

      case _ =>
        et.ProcedureCall(procedure, operands.map(extractExpression(_)))
    }
  }

  private def extractExpression(datum : ast.Datum)(implicit scope : Scope) : et.Expression = datum match {
    // Normal procedure call
    case ast.ProperList(ast.Symbol(procedureName) :: operands) =>
      extractApplication(getVar(procedureName), operands)

    case ast.Symbol(variableName) =>
      et.VarReference(getVar(variableName))

    // These all evaluate to themselves. See R7RS section 4.1.2
    case literal : ast.NumberLiteral =>
      et.Literal(literal)
    case literal : ast.StringLiteral =>
      et.Literal(literal)
    case literal : ast.CharLiteral =>
      et.Literal(literal)
    case literal : ast.Vector =>
      et.Literal(literal)
    case literal : ast.ByteVector =>
      et.Literal(literal)
    case literal : ast.BooleanLiteral =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExpressionException(malformed.toString)
  }

  def apply(data : List[ast.Datum])(implicit scope : Scope) : (List[et.Expression], Scope) = data match {
    // We're done!
    case Nil => (Nil, scope)

    case datum :: restData =>
      datum match {
        case ast.ProperList(ast.Symbol("define") :: ast.Symbol(varName) :: value :: Nil) =>
          continueBodyWithDefine(restData, varName, extractExpression(value))

        case ast.ProperList(ast.Symbol("define") :: ast.ProperList(ast.Symbol(varName) :: fixedArgs) :: body) =>
          continueBodyWithDefine(restData, varName, createLambda(fixedArgs, None, body))
        
        case ast.ProperList(ast.Symbol("define") :: ast.ImproperList(ast.Symbol(varName) :: fixedArgs, ast.Symbol(restArg)) :: body) =>
          continueBodyWithDefine(restData, varName, createLambda(fixedArgs, Some(restArg), body))
        
        case expressionDatum =>
          var expression = extractExpression(expressionDatum)(scope)

          ExtractBody(restData)(scope) match {
            case (exprs, finalScope) =>
              (expression :: exprs, finalScope)
          }
      }
  }
}
