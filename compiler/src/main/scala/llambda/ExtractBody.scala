package llambda

object ExtractBody {
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

  def apply(data : List[ast.Datum])(implicit scope : Scope) : (List[et.Expression], Scope) = data match {
    // We're done!
    case Nil => (Nil, scope)

    case datum :: restData =>
      datum match {
        case ast.ProperList(ast.Symbol("define") :: ast.Symbol(varName) :: value :: Nil) =>
          continueBodyWithDefine(restData, varName, ExtractExpressions(value))

        case ast.ProperList(ast.Symbol("define") :: ast.ProperList(ast.Symbol(varName) :: fixedArgs) :: body) =>
          continueBodyWithDefine(restData, varName, CreateLambda(fixedArgs, None, body))
        
        case ast.ProperList(ast.Symbol("define") :: ast.ImproperList(ast.Symbol(varName) :: fixedArgs, ast.Symbol(restArg)) :: body) =>
          continueBodyWithDefine(restData, varName, CreateLambda(fixedArgs, Some(restArg), body))
        
        case expressionDatum =>
          var expression = ExtractExpressions(expressionDatum)(scope)

          ExtractBody(restData)(scope) match {
            case (exprs, finalScope) =>
              (expression :: exprs, finalScope)
          }
      }
  }
}
