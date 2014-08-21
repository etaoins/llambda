package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, ContextLocated, ReportProcedure}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{ValueNotApplicableException, IncompatibleArityException}
import llambda.compiler.codegen.CostForPlanSteps

private[planner] object PlanApplication {
  private case class PlanApplyResult(
      planResult : PlanResult,
      fixedArgTypes : Seq[vt.SchemeType],
      restArgMemberTypeOpt : Option[vt.SchemeType]
  )

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
              ).planResult
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

    val applyResult = planWithOperandValues(operandState)(located, procExpr, operands)

    // If we've gotten this far we know all of our operands are of the correct type
    val postFixedArgState = operands.zip(applyResult.fixedArgTypes).foldLeft(applyResult.planResult.state) {
      case (state, ((_, fixedArgValue), argType)) =>
        val constraint = ConstrainType.IntersectType(argType)
        ConstrainType(state)(fixedArgValue, constraint)
    }

    val restOperandValues = operands.drop(applyResult.fixedArgTypes.length)
    val postRestArgState = restOperandValues.foldLeft(postFixedArgState) {
      case (state, (_, restArgValue)) =>
        val constraint = ConstrainType.IntersectType(applyResult.restArgMemberTypeOpt.get)
        ConstrainType(state)(restArgValue, constraint)
    }

    PlanResult(
      state=postRestArgState,
      value=applyResult.planResult.value
    )
  }
    
  private def planWithOperandValues(initialState : PlannerState)(
      located : ContextLocated,
      procExpr : et.Expr,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : PlanApplyResult = {
    implicit val worldPtr = initialState.worldPtr
    
    // If this is a self-executing lambda try to apply it without planning a function at all
    // The procedure expression will never be used again so there's no reason to cost the the out-of-line version
    procExpr match {
      case lambdaExpr : et.Lambda if plan.config.optimize =>
        // We can apply this inline!
        for(inlineResult <- AttemptInlineApply(initialState, initialState)(lambdaExpr, operands)) {
          return PlanApplyResult(
            planResult=PlanResult(
              state=initialState,
              value=inlineResult
            ),
            fixedArgTypes=lambdaExpr.fixedArgs.map(_.schemeType),
            restArgMemberTypeOpt=lambdaExpr.restArgOpt.map(_.memberType)
          )
        }

      case _ =>
    }

    val procResult = PlanExpr(initialState)(procExpr)

    val invokableProc = procResult.value.toInvokableProcedure() getOrElse {
      throw new ValueNotApplicableException(located, procResult.value.typeDescription)
    }

    val signature = invokableProc.signature
    
    // Ensure our arity is sane
    if (signature.restArgOpt.isDefined) {
      if (operands.length < signature.fixedArgs.length) {
        throw new IncompatibleArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${operands.length} arguments; requires at least ${signature.fixedArgs.length} arguments"
        )
      }
    }
    else {
      if (signature.fixedArgs.length != operands.length) {
        throw new IncompatibleArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${operands.length} arguments; requires exactly ${signature.fixedArgs.length} arguments"
        )
      }
    }

    // Does this procedure support planning its application inline?
    procResult.value match {
      case knownProc : iv.KnownProc =>
        for(inlineResult <- knownProc.attemptInlineApplication(procResult.state)(operands)) {
          return PlanApplyResult(
            planResult=inlineResult,
            fixedArgTypes=signature.fixedArgs.map(_.schemeType),
            restArgMemberTypeOpt=signature.restArgOpt
          )
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

            return PlanApplyResult( 
              planResult=PlanResult(
                state=procResult.state,
                value=inlineValue
              ),
              fixedArgTypes=signature.fixedArgs.map(_.schemeType),
              restArgMemberTypeOpt=signature.restArgOpt
            )
          }
        }

      case _ =>
    }

    // Use the invoke plan
    plan.steps ++= invokePlan.steps

    PlanApplyResult(
      planResult=PlanResult(
        state=procResult.state,
        value=invokeValue
      ),
      fixedArgTypes=signature.fixedArgs.map(_.schemeType),
      restArgMemberTypeOpt=signature.restArgOpt
    )
}
}
