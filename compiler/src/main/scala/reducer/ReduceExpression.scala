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

  private def findUsedVars(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(storageLoc) => 
      Set(storageLoc)

    case other =>
      other.subexpressions.flatMap(findUsedVars(_)).toSet
  }

  /**
   * Reduces an expression to the simplest form possible while preserving meaning
   */
  def apply(expr : et.Expression)(implicit reduceConfig : ReduceConfig) : et.Expression = (expr match {
    case begin : et.Begin =>
      val mappedExprs = begin.toSequence.map(apply)
      unflattenExprs(mappedExprs)

    case et.Apply(et.VarRef(reportProc : ReportProcedure), operands) =>
      val reducedOperands = operands.map(ReduceExpression(_))

      for(reportProcReducer <- reportProcReducers) {
        for(expr <- reportProcReducer(reportProc, reducedOperands)) {
          return expr
        }
      }

      // Nothing we can do
      et.Apply(et.VarRef(reportProc), reducedOperands)

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

    // Simplify self-executing lambdas without explicit returns
    case et.Apply(et.Lambda(fixedArgs, None, bodyExpr), operandExprs) if (fixedArgs.length == operandExprs.length) =>
      val reducedOperandExprs = operandExprs.map(apply)

      // We now have known values for any non-mutable args
      val knownArguments = (fixedArgs.zip(reducedOperandExprs) flatMap { case (argLoc, operandExpr) =>
        if (reduceConfig.analysis.mutableVars.contains(argLoc)) {
          None
        }
        else {
          Some(argLoc -> operandExpr)
        }
      }).toMap

      val bodyConfig = reduceConfig.copy(
        knownConstants=(reduceConfig.knownConstants ++ knownArguments)
      )

      // Reduce the body
      val reducedBodyExpr = apply(bodyExpr)(bodyConfig)

      // Now that we've reduced the body does it have any early returns?
      if (ExpressionCanReturn(reducedBodyExpr)) {
        // Yes - we can't flatten the lambda but we can reuse the work we just did
        et.Apply(et.Lambda(fixedArgs, None, reducedBodyExpr), reducedOperandExprs)
      }
      else {
        val bodyUsedVars = findUsedVars(reducedBodyExpr)

        // Assign any used operands to new storage locations
        // This is required to keep the semantics of the lambda receiving copies of its arguments
        val bindingsExprs = fixedArgs.zip(reducedOperandExprs) flatMap { case (argLoc, operandExpr) =>
          if (!bodyUsedVars.contains(argLoc) && IsPureExpression(operandExpr)) {
            // Drop this entirely
            // This is useful because things like (or) leave lots of Bind()s around otherwise and Bind() is impure
            // This prevents further reductions on the expression
            None
          }
          else {
            Some(et.Bind(List(argLoc -> operandExpr)).assignLocationFrom(expr))
          }
        }

        // We can flatten!
        et.Expression.fromSequence(bindingsExprs :+ reducedBodyExpr)
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
