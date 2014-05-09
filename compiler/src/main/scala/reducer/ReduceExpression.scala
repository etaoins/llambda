package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object ReduceExpression {
  private val reportProcReducers = List[reportproc.ReportProcReducer](
    reportproc.BooleanProcReducer,
    reportproc.CallCcProcReducer,
    reportproc.EquivalenceProcReducer,
    reportproc.NumberProcReducer,
    reportproc.ListProcReducer,
    reportproc.StringProcReducer,
    reportproc.SymbolProcReducer,
    reportproc.VectorProcReducer
  )

  private def unflattenExprs(exprs : List[et.Expression])(implicit reduceConfig : ReduceConfig) : et.Expression = exprs match {
    case Nil =>
      et.Begin(Nil)

    case nonEmptyExprs =>
      // Get rid of any pure expressions that aren't in the last position
      val nonValueExprs = nonEmptyExprs.dropRight(1)
      val valueExpr = nonEmptyExprs.last

      val impureNonValueExprs = nonValueExprs.filterNot(IsPureExpression(_))
      val newExprs = impureNonValueExprs :+ valueExpr
      
      et.Expression.fromSequence(newExprs)
  }

  /**
   * Reduces an expression to the simplest form possible while preserving meaning
   */
  def apply(expr : et.Expression)(implicit reduceConfig : ReduceConfig) : et.Expression = (expr match {
    case begin : et.Begin =>
      val mappedExprs = begin.toSequence.map(apply)
      unflattenExprs(mappedExprs)

    case et.Apply(appliedExpr @ et.VarRef(reportProc : ReportProcedure), operands) =>
      val reducedOperands = operands.map(ReduceExpression(_))

      for(reportProcReducer <- reportProcReducers) {
        for(expr <- reportProcReducer(reportProc, reducedOperands)) {
          return expr
        }
      }

      ReduceApplication(appliedExpr, reducedOperands)

    case et.Apply(appliedExpr, operands) =>
      val reducedOperands = operands.map(ReduceExpression(_))
      ReduceApplication(appliedExpr, reducedOperands)

    case et.Bind(bindings) =>
      val newBindings = (bindings.map { case (storageLoc, initializer) =>
        // Reduce the initializers first to make them pure
        storageLoc -> ReduceExpression(initializer)
      }).filter { case (storageLoc, reducedInitializer) =>
        // Drop any bindings of unused variables to pure expressions
        reduceConfig.analysis.usedVars.contains(storageLoc) || !IsPureExpression(reducedInitializer)
      }

      newBindings match {
        case Nil =>
          et.Literal(ast.UnitValue())

        case other =>
          et.Bind(newBindings)
      }


    case et.Cond(testExpr, trueExpr, falseExpr) =>
      val reducedTest = apply(testExpr)
      val testIsPure = IsPureExpression(reducedTest)
      
      // Try to optimize out the branch
      // We specially handle impure tests below
      LiteralValue(reducedTest, allowImpureExprs=true) match {
        case Some(literal) =>
          val branchExpr = if (literal == ast.BooleanLiteral(false)) {
            apply(falseExpr)
          }
          else {
            apply(trueExpr)
          }

          if (testIsPure) {
            // Just need the branch expression
            branchExpr
          }
          else {
            // Need the test and the branch expression
            unflattenExprs(
              reducedTest.toSequence ++ branchExpr.toSequence
            )
          }

        case _ =>
          // Can't statically evaluate the test
          et.Cond(
            reducedTest,
            apply(trueExpr),
            apply(falseExpr)
          )
      }

    case unknownExpr =>
      // Unknown expression - map subexpressions
      unknownExpr.map(apply)
  }).assignLocationFrom(expr)
}
