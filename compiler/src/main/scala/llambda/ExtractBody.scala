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

  def apply(data : List[ast.Datum])(implicit scope : Scope) : (List[et.Expression], Scope) = data match {
    // We're done!
    case Nil => (Nil, scope)

    case datum :: restData =>
      datum match {
        case ast.ProperList(ast.Symbol("define") :: ast.Symbol(varName) :: value :: Nil) =>
          val (storageLoc, newScope) = defineVar(varName)(scope)

          // Use the old scope when expanding the definitions
          val setExpr = et.SetVar(storageLoc, ExtractExpressions(value)(scope))
          
          ExtractBody(restData)(newScope) match {
            case (exprs, finalScope) =>
              (setExpr :: exprs, finalScope)
          }

        case expressionDatum =>
          var expression = ExtractExpressions(expressionDatum)(scope)

          ExtractBody(restData)(scope) match {
            case (exprs, finalScope) =>
              (expression :: exprs, finalScope)
          }
      }
  }
}
