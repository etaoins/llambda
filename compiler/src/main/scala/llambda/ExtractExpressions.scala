package llambda

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)
class UnboundVariableException(message : String) extends SemanticException(message)

object ExtractExpressions {
  private def getVar(name : String)(implicit scope : Scope) : BoundValue = {
    scope.get(name).getOrElse {
      throw new UnboundVariableException(name)
    }
  }

  private def extractApplication(procedure : BoundValue, operands : List[ast.Datum])(implicit scope : Scope) : et.Expression = {
    (procedure, operands) match {
      case (SchemePrimitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum)

      case (SchemePrimitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), Some(ExtractExpressions(falseExpr)))
      
      case (SchemePrimitives.If, test :: trueExpr :: Nil) =>
        et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), None)

      case (SchemePrimitives.Set, ast.Symbol(variableName) :: value :: Nil) =>
        et.SetVar(getVar(variableName), ExtractExpressions(value))

      case (SchemePrimitives.Lambda, ast.Symbol(restArg) :: body) =>
        CreateLambda(List(), Some(restArg), body)(scope)

      case (SchemePrimitives.Lambda, ast.ProperList(fixedArgData) :: body) =>
        CreateLambda(fixedArgData, None, body)

      case (SchemePrimitives.Lambda, ast.ImproperList(fixedArgData, ast.Symbol(restArg)) :: body) =>
        CreateLambda(fixedArgData, Some(restArg), body)(scope)

      case _ =>
        et.ProcedureCall(procedure, operands.map(ExtractExpressions(_)))
    }
  }

  def apply(datum : ast.Datum)(implicit scope : Scope) : et.Expression = datum match {
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
}
