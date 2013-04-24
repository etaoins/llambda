package llambda

class MalformedExpressionException(message : String) extends SemanticException(message)
class BadSpecialFormException(message : String) extends SemanticException(message)

object ExtractExpressions {
  private def requiredVarReference(datum : ast.Datum) : et.UnresolvedVar = datum match {
    case ast.Symbol(name) => et.UnresolvedVar(name)
    case _ => throw new BadSpecialFormException("Symbol expected, found " + datum)
  }

  private def parseProcedure(fixedArgs : List[ast.Datum], restArg : Option[String], body : List[ast.Datum]) : et.Procedure[et.UnresolvedVar] = {
      val fixedArgNames = fixedArgs.map(requiredVarReference(_))
      val bodyExpressions = body.map(ExtractExpressions(_))
      et.Procedure(fixedArgNames, restArg.map(et.UnresolvedVar(_)), bodyExpressions)
  }

  def apply(datum : ast.Datum) : et.Expression[et.UnresolvedVar] = datum match {
    case ast.ProperList(ast.Symbol("lambda") :: ast.Symbol(restArg) :: body) =>
      parseProcedure(List(), Some(restArg), body)
    
    case ast.ProperList(ast.Symbol("lambda") :: ast.ProperList(fixedArgData) :: body) =>
      parseProcedure(fixedArgData, None, body)
    
    case ast.ProperList(ast.Symbol("lambda") :: ast.ImproperList(fixedArgData, ast.Symbol(restArg)) :: body) =>
      parseProcedure(fixedArgData, Some(restArg), body)
    
    case ast.ProperList(ast.Symbol("define-syntax") :: ast.Symbol(keyword) ::
                         ast.ProperList(
                           ast.Symbol("syntax-rules") :: ast.ProperList(literals) :: rules
                         ) :: Nil) =>
      val literalNames = literals.map { 
        case ast.Symbol(name) => name
        case nonSymbol => throw new BadSpecialFormException("Symbol expected in literal list, found " + nonSymbol)
      }

      val parsedRules = rules.map {
        case ast.ProperList(ast.ProperList(_ :: pattern) :: template :: Nil) =>
          et.SyntaxRule(pattern, ExtractExpressions(template))
        case noMatch => throw new BadSpecialFormException("Unable to parse syntax rule " + noMatch)
      }

      et.DefineSyntax(et.UnresolvedVar(keyword), literalNames, parsedRules)

    case ast.ProperList(ast.Symbol("define") :: ast.Symbol(variable) :: value :: Nil) =>
      et.DefineVar(et.UnresolvedVar(variable), ExtractExpressions(value))
    
    case ast.ProperList(ast.Symbol("define") :: ast.ImproperList(ast.Symbol(variable) :: fixedArgs, ast.Symbol(restArg)) :: body) =>
      et.DefineVar(et.UnresolvedVar(variable), parseProcedure(fixedArgs, Some(restArg), body))
    
    case ast.ProperList(ast.Symbol("define") :: ast.ProperList(ast.Symbol(variable) :: fixedArgs) :: body) =>
      et.DefineVar(et.UnresolvedVar(variable), parseProcedure(fixedArgs, None, body))
    
    case ast.ProperList(ast.Symbol("quote") :: innerDatum :: Nil) =>
      et.Literal(innerDatum)
    
    case ast.ProperList(ast.Symbol("if") :: test :: trueExpr :: falseExpr :: Nil) =>
      et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), Some(ExtractExpressions(falseExpr)))
    
    case ast.ProperList(ast.Symbol("if") :: test :: trueExpr  :: Nil) =>
      et.Conditional(ExtractExpressions(test), ExtractExpressions(trueExpr), None)

    case ast.ProperList(ast.Symbol(name) :: operands) =>
      et.Application(et.UnresolvedVar(name), operands.map(ExtractExpressions(_)))

    case ast.Symbol(varName) =>
      et.VarReference(et.UnresolvedVar(varName))

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
