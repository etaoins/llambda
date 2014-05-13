package io.llambda.compiler.reducer
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer.{partialvalue => pv}

private[reducer] object ReduceApplication {
  private val reportProcReducers = List[reportproc.ReportProcReducer](
    reportproc.ApplyProcReducer,
    reportproc.BooleanProcReducer,
    reportproc.CallCcProcReducer,
    reportproc.EquivalenceProcReducer,
    reportproc.NumberProcReducer,
    reportproc.ListProcReducer,
    reportproc.StringProcReducer,
    reportproc.SymbolProcReducer,
    reportproc.VectorProcReducer
  )
  
  private sealed abstract class ResolvedAppliedExpr {
    def toOutOfLine : ResolvedAppliedExpr
  }

  private case class ResolvedLambda(lambdaExpr : et.Lambda, inlineDefinition : Boolean) extends ResolvedAppliedExpr {
    def toOutOfLine = this.copy(inlineDefinition=false)
  }

  private case class ResolvedReportProcedure(reportProc : ReportProcedure) extends ResolvedAppliedExpr {
    def toOutOfLine = this
  }

  /** Returns if an expression is trivial
    *
    * "Trivial" is a qualitative property which means a negligible amount of code needs to be generated for the 
    * expression. At the moment that's either variable references or constant data that cannot reference other data
    * (leaf data).
    */
  private def expressionIsTrivial(expr : et.Expression) : Boolean = expr match {
    case et.Literal(_ : ast.Leaf) | et.VarRef(_) =>
      true

    case _ =>
      false
  }
  
  /** Returns true if we should inline an inlineable expression
    *
    * This currently is either a trivial expression or an application involving only trivial expressions. Note that
    * this definition is non-recursive - i.e. applications of applications of trivial expressions are disallowed.
    */
  private def shouldInlineExpr(candidateExpr : et.Expression) : Boolean = candidateExpr match {
    case et.InternalDefinition(bindings, bodyExpr) =>
      if (bindings.map(_._2).forall(expressionIsTrivial)) {
        // Unwrap the internal definition
        shouldInlineExpr(bodyExpr)
      }
      else {
        false
      }

    case applyExpr : et.Apply =>
      applyExpr.subexpressions.forall(expressionIsTrivial)

    case otherExpr =>
      expressionIsTrivial(otherExpr)
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
    // Make sure we have the right arity
    if (lambdaExpr.restArg.isDefined) {
      if (reducedOperands.length < lambdaExpr.fixedArgs.length) {
        return None
      }
    }
    else if (reducedOperands.length != lambdaExpr.fixedArgs.length) {
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

    // Make new bindings for the fixed arguments
    // We can't create a full binding for the rest argument (yet) - we only set a known value
    val newBindings = lambdaExpr.fixedArgs.zip(reducedOperands)

    // Create partial values for all values we know
    val newFixedKnownValues = newBindings.map { case (storageLoc, operandExpr) =>
      storageLoc -> pv.PartialValue.fromReducedExpression(operandExpr)
    }

    val newRestKnownValues = (lambdaExpr.restArg map { restArgLoc =>
      // Find our rest arguments (if any)
      val restReducedOperands = reducedOperands.drop(lambdaExpr.fixedArgs.length)
      val restPartialValues = restReducedOperands.map(pv.PartialValue.fromReducedExpression(_))

      // Make a partial proper list for the rest arg
      // This allows things like (length) on the rest args, allow it to be re-packed for (apply) etc.
      val restArgPartialList = pv.ProperList(restPartialValues)

      restArgLoc -> restArgPartialList
    }).toList
    
    // Make the analysis for the inner variables
    val analysis = reduceConfig.analysis

    val innerReduceConfig = reduceConfig.copy(
      knownValues=reduceConfig.knownValues ++ newFixedKnownValues ++ newRestKnownValues,
      inlineDepth=reduceConfig.inlineDepth + 1
    )

    // Reduce the expression
    val reducedBodyExpr = ReduceExpression(lambdaExpr.body)(innerReduceConfig)

    // Can we return?
    if (ExpressionCanReturn(reducedBodyExpr)) {
      // We can't safely inline this
      return None
    }

    // Do we reduce to a single variable reference?
    // This is technically a type of eta conversion and makes our ouput cleaner
    reducedBodyExpr match {
      case et.VarRef(storageLoc) =>
        for((bindingLoc, initializer) <- newBindings) {
          if (bindingLoc == storageLoc) {
            return Some(initializer)
          }
        }

      case _ =>
    }

    val bodyUsedVars = ReferencedVariables(reducedBodyExpr)
    
    // Is the rest arg itself still referenced?
    // We refuse to manually build a rest arg at the moment so we fall back to the planner to do so
    for(restArgStorageLoc <- lambdaExpr.restArg) {
      if (bodyUsedVars.contains(restArgStorageLoc)) {
        return None
      }
    }

    if (forceInline || shouldInlineExpr(reducedBodyExpr)) {
      // Figure out which bindings are still required
      val usedBindings = newBindings filter { case (argLoc, operandExpr) =>
        bodyUsedVars.contains(argLoc) || !IsPureExpression(operandExpr)(reduceConfig)
      }

      Some(if (usedBindings.isEmpty) {
        // No bindings, just the return the reduced body
        reducedBodyExpr
      }
      else {
        // Wrap in a binding expression
        et.InternalDefinition(usedBindings, reducedBodyExpr)
      })
    }
    else {
      // Inlining is possible but determined not to be beneficial 
      None
    }
  }


  /** Resolves an applied expression attempting to find a report procedure or lambda */
  private def resolveAppliedExpr(expr : et.Expression, ignoreReportProcs : Set[ReportProcedure])(implicit reduceConfig : ReduceConfig) : Option[ResolvedAppliedExpr] = expr match {
    case et.VarRef(reportProcedure : ReportProcedure) if !ignoreReportProcs.contains(reportProcedure) =>
      Some(ResolvedReportProcedure(reportProcedure))

    case et.VarRef(storageLoc) =>
      // Dereference the variable
      val storageLocExprOpt = (reduceConfig.knownValues.get(storageLoc).flatMap(_.toExpressionOpt) orElse
        reduceConfig.analysis.constantTopLevelBindings.get(storageLoc)
      )
          
      storageLocExprOpt.flatMap { derefedExpr =>
        // This is no longer an inline definition
        resolveAppliedExpr(derefedExpr, ignoreReportProcs).map(_.toOutOfLine)
      }

    case lambdaExpr : et.Lambda =>
      Some(ResolvedLambda(lambdaExpr, true))

    case other => 
      None
  }

  /** Reduces a procedure application via optional inlining 
    *
    * @param  appliedExpr        Expression being applied
    * @param  reducedOperands    Reduced expressions of the operands of the application in application order
    * @param  ignoreReportProcs  Set of report procedures to treat as normal values. This is used internally to retry
    *                            reduction after failing to do report procedure specific reduction.
    * @return Expression to replace the original application with or None to use the original expression
    */
  def apply(appliedExpr : et.Expression, reducedOperands : List[et.Expression], ignoreReportProcs : Set[ReportProcedure] = Set())(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = {
    resolveAppliedExpr(appliedExpr, ignoreReportProcs) flatMap {
      case ResolvedReportProcedure(reportProc) =>
        // Run this through the report procedure reducers
        for(reportProcReducer <- reportProcReducers) {
          for(expr <- reportProcReducer(reportProc, reducedOperands)) {
            // This was recognized and reduced - perform no further action
            return Some(expr)
          }
        }

        // Couldn't reduce as a report procedure; retry as a normal expression
        apply(appliedExpr, reducedOperands, ignoreReportProcs + reportProc)

      case ResolvedLambda(lambdaExpr, inlineDefinition) =>
        reduceLambdaApplication(lambdaExpr, reducedOperands, inlineDefinition)(reduceConfig)
    }
  }
}
