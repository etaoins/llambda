package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, ContextLocated, ReportProcedure}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.reportproc.ReportProcPlanner
import llambda.compiler.{IncompatibleArityException, ImpossibleTypeConversionException}
import llambda.compiler.codegen.CostForPlanSteps

private[planner] object PlanApplication {
  def apply(state : PlannerState)(
      procExpr : et.Expr,
      operandExprs : List[et.Expr]
  )(implicit plan : PlanWriter) : PlanResult = {
    // Are we applying a report procedure?
    procExpr match {
      case et.VarRef(reportProc : ReportProcedure) =>
        val reportName = reportProc.reportName
        val reportProcPlanners = ReportProcPlanner.activePlanners

        // Can this report procedure be planned without fully evaluating the operand expressions?
        for(reportProcPlanner <- reportProcPlanners;
            planResult <- reportProcPlanner.planFromExprs(state)(reportName, operandExprs)) {
          return planResult
        }

      case _ =>
    }

    val initialResult = PlanResult(state, SingleValue(iv.UnitValue))

    val operandResults = operandExprs.scanLeft(initialResult) { case (prevResult, operandExpr) =>
      PlanExpr(prevResult.state)(operandExpr)
    }

    // Use the final operand's state
    val operandState = operandResults.last.state
    // Zip with the orignal operand expr so we can use it to locate exceptions related to that operand
    val operands = operandExprs.zip(operandResults.tail.map(_.values.toSingleValue()))

    planWithOperandValues(operandState)(procExpr, operands)
  }

  /** Registers the types of our values with the occurrence typing system */
  private def registerTypes(state : PlannerState)(
      procedureType : vt.ProcedureType,
      procValue : iv.IntermediateValue,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : PlannerState = {
    val constraint = ConstrainType.IntersectType(procedureType)
    val postProcState = ConstrainType(state)(procValue, constraint)(plan.config)

    val fixedArgTypes = procedureType.fixedArgTypes
    val postFixedArgState = operands.zip(fixedArgTypes).foldLeft(postProcState) {
      case (state, (fixedArgValue, argType)) =>
        val constraint = ConstrainType.IntersectType(argType)
        ConstrainType(state)(fixedArgValue, constraint)(plan.config)
    }

    val restOperandValues = operands.drop(fixedArgTypes.length)
    restOperandValues.foldLeft(postFixedArgState) {
      case (state, restArgValue) =>
        val restArgMemberType = procedureType.restArgMemberTypeOpt.get

        val constraint = ConstrainType.IntersectType(restArgMemberType)
        ConstrainType(state)(restArgValue, constraint)(plan.config)
    }
  }

  def planWithOperandValues(initialState : PlannerState)(
      procExpr : et.Expr,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : PlanResult = {
    // If this is a self-executing lambda try to apply it without planning a function at all
    // The procedure expression will never be used again so there's no reason to cost the the out-of-line version
    procExpr match {
      case lambdaExpr : et.Lambda if plan.config.optimize =>
        // We can apply this inline!
        for(inlineValues <- AttemptInlineApply(initialState, initialState)(lambdaExpr, operands)) {
          return PlanResult(
            state=initialState,
            values=inlineValues
          )
        }

      case _ =>
    }

    val procResult = PlanExpr(initialState)(procExpr)
    val procValue = procResult.values.toSingleValue().toApplicableValueForArity(operands.length)

    val invokableProc = procValue.toInvokableProcedure()

    val procedureType = procValue.schemeType match {
      case procType : vt.ProcedureType =>
        procType

      case other =>
        invokableProc.signature.toSchemeProcedureType
    }

    // Ensure our arity is sane
    if (procedureType.restArgMemberTypeOpt.isDefined) {
      if (operands.length < procedureType.fixedArgTypes.length) {
        throw new IncompatibleArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${operands.length} arguments; requires at least ${procedureType.fixedArgTypes.length} arguments"
        )
      }
    }
    else if (procedureType.fixedArgTypes.length != operands.length) {
      throw new IncompatibleArityException(
        located=plan.activeContextLocated,
        message=s"Called procedure with ${operands.length} arguments; requires exactly ${procedureType.fixedArgTypes.length} arguments"
      )
    }

    // Does this procedure support planning its application inline?
    procValue match {
      case knownProc : iv.KnownProc =>
        for(inlineResult <- knownProc.attemptInlineApplication(procResult.state)(operands)) {
          // ReportProcPlanners rarely contrain the types of their arguments. Handle this for them.
          return inlineResult.copy(
            state=registerTypes(inlineResult.state)(procedureType, procValue, operands.map(_._2))
          )
        }

      case _ =>
    }

    // Plan this as a an invoke (function call)
    val invokePlan = plan.forkPlan()

    val invokeValues = PlanInvokeApply.withIntermediateValues(invokableProc, operands)(invokePlan)

    if (plan.config.optimize && (initialState.inlineDepth < 8)) {
      procValue match {
        case schemeProc : iv.KnownSchemeProc if !schemeProc.recursiveSelfLoc.isDefined =>
          // Try to plan this as in inline app[lication
          val inlinePlan = plan.forkPlan()

          val inlineValuesOpt = AttemptInlineApply(schemeProc.parentState, procResult.state)(
            lambdaExpr=schemeProc.lambdaExpr,
            operands=operands
          )(inlinePlan)

          for(inlineValues <- inlineValuesOpt) {
            val inlineCost = CostForPlanSteps(inlinePlan.steps.toList)
            val invokeCost = CostForPlanSteps(invokePlan.steps.toList)

            if (inlineCost <= invokeCost) {
              // Use the inline plan
              plan.steps ++= inlinePlan.steps

              return PlanResult(
                // No need to register types here - inlining wouldn't have succeeded if the types weren't already
                // matches
                state=procResult.state,
                values=inlineValues
              )
            }
          }

        case _ =>
      }
    }

    // Use the invoke plan
    plan.steps ++= invokePlan.steps

    return PlanResult(
      state=registerTypes(procResult.state)(procedureType, procValue, operands.map(_._2)),
      values=invokeValues.withReturnType(procedureType.returnType)
    )
  }
}
