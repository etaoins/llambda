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

    case et.VarRef(storageLoc) =>
      // We should know this already unless it's recursive or mutable
      reduceConfig.knownValues.get(storageLoc)

    case expr =>
      Some(pv.PartialValue.fromReducedExpr(expr))
  }

}
