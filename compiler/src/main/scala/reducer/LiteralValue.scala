package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object LiteralValue {
  /**
   * Attempts to find the literal value of a given expression
   *
   * @param expr              Expression to examine
   * @param allowImpureExprs  Attempt to find the value of an impure expression. If this is set to true callers must
   *                          be extremely careful not to optimize out the impure expressions inside expr after finding
   *                          a literal value.
   *
   * This works by following references to constant variables and reducing the expression until a constant or
   * unreducable expression is encountered.
   */
  def apply(expr : et.Expression, allowImpureExprs : Boolean = false)(implicit reduceConfig : ReduceConfig) : Option[ast.Datum] = expr match {
    case et.Begin(Nil) =>
      Some(ast.UnitValue())
    
    case et.Begin(List(singleExpr)) =>
      LiteralValue(singleExpr, allowImpureExprs)

    case et.Begin(multipleExprs) =>
      if (allowImpureExprs || IsPureExpression(expr)) {
        // Only the last value is meaningful
        LiteralValue(multipleExprs.last, allowImpureExprs)
      }
      else {
        None
      }

    case et.Literal(datum) =>
      // This is already literal
      Some(datum)

    case et.VarRef(storageLoc) if !reduceConfig.resolvingInitializers.contains(storageLoc) =>
      // Return the initializer
      // Use a blank state here because we have no idea which context the original initializer was in
      // Also flag we're already resolving this to avoid recursion
      val initializerConfig = reduceConfig.copy(
        knownConstants=Map(),
        resolvingInitializers=reduceConfig.resolvingInitializers + storageLoc
      )
  
      reduceConfig.constantExprForStorageLoc(storageLoc) flatMap { initializer =>
        // Allow impure expressions because there's no risk of the caller optimizing the initializer out
        LiteralValue(ReduceExpression(initializer)(initializerConfig), true)(initializerConfig)
      }

    case _ =>
      None
  }

}
