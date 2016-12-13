package io.llambda.compiler.analyser
import io.llambda

import llambda.compiler._

private[analyser] object ExprHasSideEffects extends ((et.Expr) => Boolean) {
  def apply(expr: et.Expr): Boolean = expr match {
    case _: et.VarRef =>
      false

    case _: et.Literal =>
      false

    case _: et.Lambda | _: et.CaseLambda | _: et.NativeFunction | _: et.ArtificialProcedure =>
      // Procedure definitions themselves are always pure
      false

    case et.Apply(et.VarRef(stdlibProc: StdlibProcedure), args) =>
      args.exists(ExprHasSideEffects) ||
        StdlibProcHasSideEffects(stdlibProc.stdlibName, args.length)

    case et.Apply(lambdaExpr: et.Lambda, args) =>
      args.exists(ExprHasSideEffects) ||
        ExprHasSideEffects(lambdaExpr.body)

    case et.Apply(nativeFunc: et.NativeFunction, args) =>
      args.exists(ExprHasSideEffects) ||
        codegen.RuntimeFunctions.hasSideEffects(nativeFunc.nativeSymbol, args.length)

    case apply: et.Apply =>
      true

    case _: et.MutateVar =>
      true

    case _: et.TopLevelDefine =>
      // Top level definitions have the side effect of making values live for the rest of the program
      // We try to remove unused bindings so any remaining ones are likely legitimate
      true

    case internalDefine: et.InternalDefine =>
      // Internal definitions are pure as long as all the bound values and body expressions are pure
      internalDefine.subexprs.exists(ExprHasSideEffects)

    case et.Parameterize(parameterValues, bodyExpr) =>
      ExprHasSideEffects(bodyExpr) ||
        parameterValues.exists({ case (parameterExpr, valueExpr) =>
          ExprHasSideEffects(parameterExpr) || ExprHasSideEffects(valueExpr)
        })

    case _: et.Cast =>
      // Cast can cause runtime errors
      true

    case et.Cond(testExpr, trueExpr, falseExpr) =>
      ExprHasSideEffects(testExpr) ||
        ExprHasSideEffects(trueExpr) ||
        ExprHasSideEffects(falseExpr)

    case et.Begin(subexprs) =>
      subexprs.exists(ExprHasSideEffects)
  }
}
