package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object IsPureExpression {
  def apply(expr : et.Expression)(implicit reduceConfig : ReduceConfig) : Boolean = expr match {
    case et.VarRef(storageLoc) =>
      // The value of a mutable var can change depending on when it's referenced
      // This has no side effects but is not referentially transparent
      !reduceConfig.analysis.mutableVars.contains(storageLoc)

    case _ : et.Literal =>
      true

    case _ : et.Lambda | _ : et.NativeFunction | _ : et.RecordTypeProcedure =>
      // Procedure definitions themselves are always pure
      true

    case _ : et.Apply =>
      // XXX: Report procs and lambdas can be pure
      // It may be worth investigating if their purity can be significantly useful for reduction
      false

    case _ : et.MutateVar =>
      false

    case et.TopLevelDefinition(bindings) =>
      // Top level definitions have the side effect of making values live for the rest of the program
      // We try to remove unused bindings so any remaining ones are likely legitimate
      bindings.isEmpty

    case internalDefine : et.InternalDefinition =>
      // Internal definitions are pure as long as all the bound values and body expressions are pure
      internalDefine.subexpressions.forall(IsPureExpression(_))

    case _ : et.Return =>
      // Returns have the side effect of causing control flow
      // We try to avoid producing them whenever possible so if there's one in the ET it's probably required
      false

    case _ : et.Parameterize =>
      // Parameterize can call converter procedures implicitly
      // For that reason it's very difficult to determine if they're pure
      false

    case _ : et.Cast =>
      // Cast can cause runtime errors
      false

    case et.Cond(testExpr, trueExpr, falseExpr) =>
      apply(testExpr) && apply(trueExpr) && apply(falseExpr)

    case et.Begin(subexprs) =>
      subexprs.forall(apply(_))
  }
}
