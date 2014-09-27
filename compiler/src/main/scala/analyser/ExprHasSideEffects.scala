package io.llambda.compiler.analyser
import io.llambda

import io.llambda.compiler._

private[analyser] object ExprHasSideEffects extends ((et.Expr) => Boolean) {
  def apply(expr : et.Expr) : Boolean = expr match {
    case _ : et.VarRef =>
      false

    case _ : et.Literal =>
      false

    case _ : et.Lambda | _ : et.CaseLambda | _ : et.NativeFunction | _ : et.ArtificialProcedure =>
      // Procedure definitions themselves are always pure
      false
    
    case et.Apply(et.VarRef(reportProc : ReportProcedure), operands) =>
      operands.exists(ExprHasSideEffects) ||
        ReportProcHasSideEffects(reportProc.reportName, operands.length)

    case et.Apply(nativeFunc : et.NativeFunction, operands) =>
      operands.exists(ExprHasSideEffects) ||
        codegen.RuntimeFunctions.hasSideEffects(nativeFunc.nativeSymbol, operands.length)

    case apply : et.Apply =>
      true

    case _ : et.MutateVar =>
      true

    case et.TopLevelDefine(bindings) =>
      // Top level definitions have the side effect of making values live for the rest of the program
      // We try to remove unused bindings so any remaining ones are likely legitimate
      !bindings.isEmpty

    case internalDefine : et.InternalDefine =>
      // Internal definitions are pure as long as all the bound values and body expressions are pure
      internalDefine.subexprs.exists(ExprHasSideEffects)

    case _ : et.Return =>
      // Returns have the side effect of causing control flow
      // We try to avoid producing them whenever possible so if there's one in the ET it's probably required
      true

    case _ : et.Parameterize =>
      // Parameterize can call converter procedures implicitly
      // For that reason it's very difficult to determine if they're pure
      true

    case _ : et.Cast =>
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
