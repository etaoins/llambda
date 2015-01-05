package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, ContextLocated, ReportProcedure}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.reportproc.ReportProcPlanner
import llambda.compiler.ArityException
import llambda.compiler.codegen.CostForPlanSteps

private[planner] object PlanApplication {
  def apply(state : PlannerState)(
      procExpr : et.Expr,
      argExprs : List[et.Expr]
  )(implicit plan : PlanWriter) : PlanResult = {
    // Are we applying a report procedure?
    procExpr match {
      case et.VarRef(reportProc : ReportProcedure) =>
        val reportName = reportProc.reportName
        val reportProcPlanners = ReportProcPlanner.activePlanners

        // Can this report procedure be planned without fully evaluating the argument expressions?
        for(reportProcPlanner <- reportProcPlanners;
            planResult <- reportProcPlanner.planFromExprs(state)(reportName, argExprs)) {
          return planResult
        }

      case _ =>
    }

    val initialResult = PlanResult(state, SingleValue(iv.UnitValue))

    val argResults = argExprs.scanLeft(initialResult) { case (prevResult, argExpr) =>
      PlanExpr(prevResult.state)(argExpr)
    }

    // Use the final argument's state
    val argState = argResults.last.state
    // Zip with the original argument expr so we can use it to locate exceptions related to that argument
    val args = argExprs.zip(argResults.tail.map(_.values.toSingleValue()))

    planWithArgValues(argState)(procExpr, args)
  }

  /** Registers the types of our values with the occurrence typing system */
  private def registerTypes(state : PlannerState)(
      procedureType : vt.ProcedureType,
      procValue : iv.IntermediateValue,
      args : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : PlannerState = {
    val constraint = ConstrainType.IntersectType(procedureType)
    val postProcState = ConstrainType(state)(procValue, constraint)(plan.config)

    val fixedArgTypes = procedureType.fixedArgTypes
    val postFixedArgState = args.zip(fixedArgTypes).foldLeft(postProcState) {
      case (state, (fixedArgValue, argType)) =>
        val constraint = ConstrainType.IntersectType(argType)
        ConstrainType(state)(fixedArgValue, constraint)(plan.config)
    }

    val restArgValues = args.drop(fixedArgTypes.length)
    restArgValues.foldLeft(postFixedArgState) {
      case (state, restArgValue) =>
        val restArgMemberType = procedureType.restArgMemberTypeOpt.get

        val constraint = ConstrainType.IntersectType(restArgMemberType)
        ConstrainType(state)(restArgValue, constraint)(plan.config)
    }
  }

  def planWithArgValues(initialState : PlannerState)(
      procExpr : et.Expr,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : PlanResult = {
    // If this is a self-executing lambda try to apply it without planning a function at all
    // The procedure expression will never be used again so there's no reason to cost the the out-of-line version.
    // Perform this inlining at all optimisation levels because it can propagate important type information
    procExpr match {
      case lambdaExpr : et.Lambda =>
        // We can apply this inline!
        for(inlineValues <- AttemptInlineApply.fromSEL(initialState)(lambdaExpr, args)) {
          return PlanResult(
            state=initialState,
            values=inlineValues
          )
        }

      case _ =>
    }

    val procResult = PlanExpr(initialState)(procExpr)
    val procValue = procResult.values.toSingleValue().toApplicableValueForArgs(args.map(_._2.schemeType))

    val invokableProc = procValue.toInvokableProcedure()

    // Resolve our polymorphic procedure type
    val argTypes = args.map(_._2.schemeType)
    val signature = invokableProc.polySignature.signatureForArgs(plan.activeContextLocated, argTypes)

    val procedureType = signature.toSchemeProcedureType

    // Ensure our arity is sane
    if (procedureType.restArgMemberTypeOpt.isDefined) {
      if (args.length < procedureType.fixedArgTypes.length) {
        throw new ArityException(
          located=plan.activeContextLocated,
          message=s"Called procedure with ${args.length} arguments; requires at least ${procedureType.fixedArgTypes.length} arguments"
        )
      }
    }
    else if (procedureType.fixedArgTypes.length != args.length) {
      throw new ArityException(
        located=plan.activeContextLocated,
        message=s"Called procedure with ${args.length} arguments; requires exactly ${procedureType.fixedArgTypes.length} arguments"
      )
    }

    // Does this procedure support planning its application inline?
    procValue match {
      case knownProc : iv.KnownProc =>
        for(inlineResult <- knownProc.attemptInlineApplication(procResult.state)(args)) {
          // ReportProcPlanners rarely contrain the types of their arguments. Handle this for them.
          return inlineResult.copy(
            state=registerTypes(inlineResult.state)(procedureType, procValue, args.map(_._2))
          )
        }

      case _ =>
    }

    // Plan this as an invoke (function call)
    val invokePlan = plan.forkPlan()

    val invokeValues = PlanInvokeApply.withIntermediateValues(invokableProc, args)(invokePlan)

    if (plan.config.optimise && (initialState.inlineDepth < 8)) {
      procValue match {
        case schemeProc : iv.KnownSchemeProc if !schemeProc.manifest.isRecursive =>
          // Try to plan this as in inline app[lication
          val inlinePlan = plan.forkPlan()

          val inlineValuesOpt = AttemptInlineApply.fromManifiest(procResult.state)(
            manifest=schemeProc.manifest,
            args=args,
            selfTempOpt=schemeProc.selfTempOpt
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
      state=registerTypes(procResult.state)(procedureType, procValue, args.map(_._2)),
      values=invokeValues.withReturnType(procedureType.returnType)
    )
  }

  private def listToIntermediates(
      listHeadValue : iv.IntermediateValue
  )(implicit plan : PlanWriter) : Option[List[iv.IntermediateValue]] =
    listHeadValue.schemeType match {
      case _ : vt.SpecificPairType =>
        // Note that this will do the right thing with KnownListElement instances - no actual load will be performed
        listToIntermediates(PlanCadr.loadCdr(listHeadValue)) map { tailValues =>
          PlanCadr.loadCar(listHeadValue) :: tailValues
        }

      case vt.EmptyListType => Some(Nil)
      case _ => None
    }

  def planWithArgList(initialState : PlannerState)(
      procExpr : et.Expr,
      argList : iv.IntermediateValue
  )(implicit plan : PlanWriter) : PlanResult = {
    val loadValuesPlan = plan.forkPlan()

    listToIntermediates(argList)(loadValuesPlan) match {
      case Some(argValues) =>
        // We have a known number of arguments with known types - this is enough to do compile time arity checks,
        // lambda inlining etc.
        plan.steps ++= loadValuesPlan.steps

        planWithArgValues(initialState)(
          procExpr,
          argValues.map((procExpr, _))
        )

      case None =>
        val procResult = PlanExpr(initialState)(procExpr)
        val invokableProc = plan.withContextLocation(procExpr) {
          procResult.values.toSingleValue.toInvokableProcedure
        }

        PlanResult(
          state=procResult.state,
          PlanInvokeApply.withArgumentList(invokableProc, argList)
        )
    }
  }
}
