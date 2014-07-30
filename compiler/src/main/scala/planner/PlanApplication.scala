package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, ContextLocated, ReportProcedure}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.ValueNotApplicableException
import llambda.compiler.codegen.CostForPlanSteps

private[planner] object PlanApplication {
  def apply(initialState : PlannerState)(located : ContextLocated, procExpr : et.Expr, operandExprs : List[et.Expr])(implicit plan : PlanWriter) : PlanResult = {
    implicit val worldPtr = initialState.worldPtr

    // Are we applying (apply)?
    (procExpr, operandExprs) match {
      case (et.VarRef(applyProc : ReportProcedure), List(applyProcExpr, applyArgsExpr)) if applyProc.reportName == "apply" =>
        // Plan this on a separate plan so we don't double plan applyArgsExpr if we fail
        val staticApplyPlan = plan.forkPlan()

        // Don't evaluate applyProcExpr - it could be an inline lambda like (case-lambda) generates
        // We want to inline it if at all possible
        val applyArgsResult = PlanExpr(initialState)(applyArgsExpr)(staticApplyPlan)

        applyArgsResult.value match {
          case knownListElement : iv.KnownListElement =>
            for(argValues <- knownListElement.toValueList) {
              // We statically know our arguments!
              val locatedArgValues = argValues.map((applyArgsExpr, _))

              plan.steps ++= staticApplyPlan.steps
              return planWithOperandValues(applyArgsResult.state)(
                located, 
                applyProcExpr,
                locatedArgValues
              )
            }

          case other =>
            // Not a known list
        }

      case _ =>
        // Not (apply)
    }

    val initialResult = PlanResult(state=initialState, value=iv.UnitValue)

    val operandResults = operandExprs.scanLeft(initialResult) { case (prevResult, operandExpr) =>
      PlanExpr(prevResult.state)(operandExpr)
    }

    // Use the final operand's state
    val operandState = operandResults.last.state
    // Zip with the orignal operand expr so we can use it to locate exceptions related to that operand
    val operands = operandExprs.zip(operandResults.tail.map(_.value))
    
    // If this is a self-executing lambda try to apply it without planning a function at all
    // The procedure expression will never be used again so there's no reason to cost the the out-of-line version
    procExpr match {
      case lambdaExpr : et.Lambda if plan.config.optimize =>
        // We can apply this inline!
        for(inlineResult <- AttemptInlineApply(operandState, operandState)(lambdaExpr, operands)) {
          return PlanResult(
            state=operandState,
            value=inlineResult
          )
        }

      case _ =>
    }

    planWithOperandValues(operandState)(located, procExpr, operands)
  }
    
  def planWithOperandValues(initialState : PlannerState)(
      located : ContextLocated,
      procExpr : et.Expr,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : PlanResult = {
    implicit val worldPtr = initialState.worldPtr

    val procResult = PlanExpr(initialState)(procExpr)

    val invokableProc = procResult.value.toInvokableProcedure() getOrElse {
      throw new ValueNotApplicableException(located, procResult.value.typeDescription)
    }
    
    // Does this procedure support planning its application inline?
    procResult.value match {
      case knownProc : iv.KnownProc if plan.config.optimize =>
        for(inlineResult <- knownProc.attemptInlineApplication(procResult.state)(operands)) {
          return inlineResult
        }

      case _ => 
    }

    // Plan this as a an invoke (function call)
    val invokePlan = plan.forkPlan()

    val invokeValue = (invokePlan.withContextLocation(located) {
      PlanInvokeApply(invokableProc, operands)(invokePlan, worldPtr) 
    }).getOrElse(iv.UnitValue)

    procResult.value match {
      case schemeProc : iv.KnownSchemeProc if plan.config.optimize && !schemeProc.recursiveSelfLoc.isDefined =>
        // Try to plan this as in inline app[lication
        val inlinePlan = plan.forkPlan()

        val inlineValueOpt = AttemptInlineApply(schemeProc.parentState, procResult.state)(
          lambdaExpr=schemeProc.lambdaExpr,
          operands=operands
        )(inlinePlan, worldPtr) 

        for(inlineValue <- inlineValueOpt) {
          val inlineCost = CostForPlanSteps(inlinePlan.steps.toList)
          val invokeCost = CostForPlanSteps(invokePlan.steps.toList)

          if (inlineCost < invokeCost) {
            // Use the inline plan
            plan.steps ++= inlinePlan.steps

            return PlanResult(
              state=procResult.state,
              value=inlineValue
            )
          }
        }

      case _ =>
    }

    // Use the invoke plan
    plan.steps ++= invokePlan.steps

    PlanResult(
      state=procResult.state,
      value=invokeValue
    )
  }
}
