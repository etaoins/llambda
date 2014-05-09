package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._

private[reducer] object ReduceApplication {
  /** Remaps constant storage locations in an expression and its subexpressions
    *
    * @param  expr               Expression to perform the renames on
    * @param  storageLocRenames  Map of original storage locations to their renamed equivalents. If a storage location
    *                            is not found in the map its identity is preserved
    * @return Renamed expression
    */
  private def renameVariables(expr : et.Expression, storageLocRenames : Map[StorageLocation, StorageLocation]) : et.Expression = (expr match {
    case et.VarRef(storageLoc) =>
      et.VarRef(storageLocRenames.get(storageLoc).getOrElse(storageLoc))

    case otherExpr =>
      otherExpr.map(renameVariables(_, storageLocRenames))
  }).assignLocationFrom(expr)

  /** Returns if an expression is trivial
    *
    * "Trivial" is a qualitative property which means a negligible amount of code needs to be generated for the 
    * expression. At the moment that's either variable references or constant data that cannot reference other data
    * (leaf data).
    */
  private def expressionIsTrivial(expr : et.Expression) : Boolean = expr match {
    case et.Literal(_ : ast.Leaf) => true
    case et.VarRef(_) =>             true
    case _ =>                        false
  }
  
  /** Returns true if we should inline an inlineable expression
    *
    * This currently is either a trivial expression or an application involving only trivial expressions. Note that
    * this definition is non-recursive - i.e. applications of applications of trivial expressions are disallowed.
    */
  private def shouldInlineExpr(expr : et.Expression) : Boolean = expr match {
    case application : et.Apply  =>
      application.subexpressions.forall(expressionIsTrivial)

    case otherExpr =>
      expressionIsTrivial(otherExpr)
  }

  /** Finds all variables used by an expression and its subexpressions */ 
  private def findUsedVars(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(storageLoc) => 
      Set(storageLoc)

    case other =>
      other.subexpressions.flatMap(findUsedVars(_)).toSet
  }

  /** Reduces a lambda application via optional inlining 
    *
    * @param  lambdaExpr       Lambda expression of the procedure being applied
    * @param  reducedOperands  Reduced expressions of the operands of the application in application order
    * @param  forceInline      If true inlining will always be performed if possible. This is intended to force
    *                          inlining for single-use procedures where bloating generated code isn't a concern.
    * @return Defined option for the expression to replace the application with or None if inlining should not be 
    *         performed
    */
  private def reduceLambdaApplication(lambdaExpr : et.Lambda, reducedOperands : List[et.Expression], forceInline : Boolean)(reduceConfig : ReduceConfig) : Option[et.Expression] = {
    if (lambdaExpr.restArg.isDefined ||
        (lambdaExpr.fixedArgs.length != reducedOperands.length)) {
      // Not implemented
      return None
    }

    if (lambdaExpr.fixedArgs.exists(reduceConfig.analysis.mutableVars.contains(_))) {
      // Not implemented
      return None
    }
    
    if (reduceConfig.inlineDepth > 5) {
      // Too deep
      return None
    }

    // Rename all of the arguments to new storage locations
    val argLocRenames = (lambdaExpr.fixedArgs.map { argLocation =>
      argLocation -> new StorageLocation(argLocation.sourceName)
    }).toMap

    val renamedBody = renameVariables(lambdaExpr.body, argLocRenames)

    // Make new bindings for the arguments
    val newBindings = (lambdaExpr.fixedArgs.zip(reducedOperands) map { case (argLocation, operandExpr) =>
      argLocRenames(argLocation) -> operandExpr
    }) : List[(StorageLocation, et.Expression)]

    // Make the analysis for the inner variables
    val analysis = reduceConfig.analysis

    val innerReduceConfig = reduceConfig.copy(
      knownConstants=reduceConfig.knownConstants ++ newBindings,
      inlineDepth=reduceConfig.inlineDepth + 1
    )

    // Reduce the expression
    val reducedBodyExpr = ReduceExpression(renamedBody)(innerReduceConfig)
      
    // Can we return?
    if (ExpressionCanReturn(reducedBodyExpr)) {
      // We can't safely inline this
      return None
    }

    val bodyUsedVars = findUsedVars(reducedBodyExpr)

    // Assign any used operands to new storage locations
    // This is required to keep the semantics of the lambda receiving copies of its arguments
    val bindingsExprs = newBindings flatMap { case (argLoc, operandExpr) =>
      if (!bodyUsedVars.contains(argLoc) && IsPureExpression(operandExpr)(reduceConfig)) {
        // Drop this entirely
        // This is useful because things like (or) leave lots of Bind()s around otherwise and Bind() is impure
        // This prevents further reductions on the expression
        None
      }
      else {
        Some(et.Bind(List(argLoc -> operandExpr)).assignLocationFrom(lambdaExpr))
      }
    }

    if (forceInline || shouldInlineExpr(reducedBodyExpr)) {
      Some(
        et.Expression.fromSequence(bindingsExprs :+ reducedBodyExpr)
      )
    }
    else {
      // Inlining is possible but determined not to be beneficial 
      None
    }
  }

  /** Reduces a procedure application via optional inlining 
    *
    * @param  appliedExpr      Expression being applied
    * @param  reducedOperands  Reduced expressions of the operands of the application in application order
    * @return Expression to replace the original application with
    */
  def apply(appliedExpr : et.Expression, reducedOperands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : et.Expression = {
    case class AppliedExpressionData(
      lambdaExpr : et.Lambda,
      forceInline : Boolean
    )

    val appliedExprDataOpt = appliedExpr match {
      case et.VarRef(storageLoc) =>
        // Does this point to a lambda?
        // We can't inline other types of functions
        (reduceConfig.constantExprForStorageLoc(storageLoc).collect {
          case lambdaExpr : et.Lambda => 
            // Don't inline this - that could cause unbounded code growth
            AppliedExpressionData(lambdaExpr, false)
        })

      case lambdaExpr : et.Lambda =>
        // This is a self-executing lambda
        // Do whatever we can to inline - this is the only use
        Some(AppliedExpressionData(lambdaExpr, true))

      case _ =>
        None
    }
      
    val reducedLambdaOpt = appliedExprDataOpt.flatMap {
      case AppliedExpressionData(lambdaExpr, forceInline) =>
        reduceLambdaApplication(lambdaExpr, reducedOperands, forceInline)(reduceConfig)
    }

    reducedLambdaOpt.getOrElse {
      // Use the original apply with our reduced operands
      et.Apply(appliedExpr, reducedOperands)
    }
  }
}
