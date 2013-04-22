package llambda

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)

object ExtractExpressions {
  private def requiredSymbolName(datum : ast.Datum) : String = datum match {
    case ast.Symbol(name) => name
    case _ => throw new BadSpecialFormException("Symbol expected, found " + datum)
  }

  def apply(datum : ast.Datum) : et.Expression = datum match {
    case ast.ProperList(ast.Symbol("lambda") :: ast.Symbol(restArg) :: body) =>
      val bodyExpressions = body.map(ExtractExpressions(_))
      et.Procedure(List(), Some(restArg), bodyExpressions)
    
    case ast.ProperList(ast.Symbol("lambda") :: ast.ProperList(fixedArgData) :: body) =>
      val fixedArgNames = fixedArgData.map(requiredSymbolName(_))
      val bodyExpressions = body.map(ExtractExpressions(_))
      et.Procedure(fixedArgNames, None, bodyExpressions)
    
    case ast.ProperList(ast.Symbol("lambda") :: ast.ImproperList(fixedArgData, ast.Symbol(restArg)) :: body) =>
      val fixedArgNames = fixedArgData.map(requiredSymbolName(_))
      val bodyExpressions = body.map(ExtractExpressions(_))
      et.Procedure(fixedArgNames, Some(restArg), body.map(ExtractExpressions(_)))
    
    case ast.ProperList(ast.Symbol("quote") :: innerDatum :: Nil) =>
      et.Literal(innerDatum)
    
    case ast.ProperList(ast.Symbol("if") :: test :: trueExpr :: falseExpr :: Nil) =>
      et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), Some(ExtractExpressions(falseExpr)))
    
    case ast.ProperList(ast.Symbol("if") :: test :: trueExpr  :: Nil) =>
      et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), None)

    case ast.ProperList(ast.Symbol(name) :: operands) =>
      et.Application(name, operands.map(ExtractExpressions(_)))

    case ast.Symbol(varName) =>
      et.VarReference(varName)

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
