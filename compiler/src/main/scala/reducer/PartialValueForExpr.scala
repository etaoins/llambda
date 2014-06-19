package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

private[reducer] object PartialValueForExpr {
  /**
   * Attempts to find the literal value of a given reduced expression
   *
   * @param expr              Expr to examine
   * @param allowImpureExprs  Attempt to find the value of an impure expression. If this is set to true callers must
   *                          be extremely careful not to optimize out the impure expressions inside expr after finding
   *                          a literal value.
   *
   * This works by following references to constant variables and reducing the expression until a constant or
   * unreducable expression is encountered.
   */
  def apply(expr : et.Expr, allowImpureExprs : Boolean = false)(implicit reduceConfig : ReduceConfig) : Option[pv.PartialValue] = expr match {
    case et.Begin(Nil) =>
      Some(pv.PartialValue.fromDatum(ast.UnitValue()))
    
    case et.Begin(List(singleExpr)) =>
      PartialValueForExpr(singleExpr, allowImpureExprs)

    case et.Begin(multipleExprs) =>
      if (allowImpureExprs || !ExprHasSideEffects(expr)) {
        // Only the last value is meaningful
        PartialValueForExpr(multipleExprs.last, allowImpureExprs)
      }
      else {
        None
      }

    case et.InternalDefine(_, expr) =>
      PartialValueForExpr(expr, allowImpureExprs)

    case et.Literal(datum) =>
      // This is already literal
      Some(pv.PartialValue.fromDatum(datum))

    case et.VarRef(storageLoc) if !reduceConfig.resolvingInitializers.contains(storageLoc) =>
      // Return the initializer
      // Use a blank state here because we have no idea which context the original initializer was in
      // Also flag we're already resolving this to avoid recursion
      val initializerConfig = reduceConfig.copy(
        knownValues=Map(),
        resolvingInitializers=reduceConfig.resolvingInitializers + storageLoc
      )
  
      // Do we already know this partial value?
      reduceConfig.knownValues.get(storageLoc) orElse {
        // No, check if it has an initialiser
        // This will only succeed if we're doing our pre-reduction pass on top level bindings
        val initializerOpt = reduceConfig.analysis.constantTopLevelBindings.get(storageLoc)
        initializerOpt.flatMap({ initializer =>
          // Allow impure expressions because there's no risk of the caller optimizing the initializer out
          PartialValueForExpr(
            ReduceExpr(initializer)(initializerConfig), true
          )(initializerConfig)
        })
      }

    case expr =>
      Some(pv.PartialValue.fromReducedExpr(expr))
  }

}
