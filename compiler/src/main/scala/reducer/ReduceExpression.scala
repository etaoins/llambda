package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

private[reducer] object ReduceExpression {
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

    case et.Apply(appliedExpr, operands) =>
      val reducedOperands = operands.map(ReduceExpression(_))

      ReduceApplication(appliedExpr, reducedOperands) getOrElse {
        // We declined reducing this application
        et.Apply(apply(appliedExpr), reducedOperands)  
      }

    case et.TopLevelDefinition(bindings) =>
      // Reduce our bindings and drop unused pure bindings
      val usedBindings = (bindings.map { case (storageLoc, initializer) =>
        storageLoc -> ReduceExpression(initializer)
      }).filter { case (storageLoc, reducedInitializer) =>
        // Drop any bindings of unused variables to pure expressions
        reduceConfig.analysis.usedVars.contains(storageLoc) || !IsPureExpression(reducedInitializer)
      }

      usedBindings match {
        case Nil =>
          et.Literal(ast.UnitValue())

        case nonEmptyBindings =>
          et.TopLevelDefinition(nonEmptyBindings)
      }

    case et.InternalDefinition(bindings, bodyExpr) =>
      // Just reduce our bindings first
      val reducedBindings = bindings.map { case (storageLoc, initializer) =>
        storageLoc -> ReduceExpression(initializer)
      }
      
      // We now have known values for the body expression
      val newKnownValues = reducedBindings.filter({ case (storageLoc, reducedInitializer) =>
        !reduceConfig.analysis.mutableVars.contains(storageLoc)
      }).map { case (storageLoc, reducedInitializer) =>
        storageLoc -> pv.PartialValue.fromReducedExpression(reducedInitializer)
      }
      
      val bodyConfig = reduceConfig.copy(
        knownValues=(reduceConfig.knownValues ++ newKnownValues)
      )

      // Reduce the body
      val reducedBody = ReduceExpression(bodyExpr)(bodyConfig)

      // Strip unused bindings
      // Note that unlike lambdas we need to handle recursively defined values in our bindings
      val bodyUsedVars = ReferencedVariables(reducedBody)    
      val recursiveBindingUsedVars = (reducedBindings.flatMap { case (_, initializer) =>
        ReferencedVariables(initializer)
      }).toSet

      val allUsedVars = bodyUsedVars ++ recursiveBindingUsedVars 

      val usedBindings = reducedBindings.filter { case (storageLoc, initializer) =>
        allUsedVars.contains(storageLoc) || !IsPureExpression(initializer)
      }
       
      if (usedBindings.isEmpty) {
        // We can drop the InternalDefinition entirely and use the unwrapped body
        reducedBody
      }
      else {
        et.InternalDefinition(usedBindings, reducedBody)
      }

    case et.Cond(testExpr, trueExpr, falseExpr) =>
      val reducedTest = apply(testExpr)
      val testIsPure = IsPureExpression(reducedTest)
      
      // Try to optimize out the branch
      // We specially handle impure tests below
      LiteralForExpression(reducedTest, allowImpureExprs=true) match {
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
