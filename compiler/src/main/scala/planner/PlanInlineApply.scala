package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ContextLocated}
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}


private[planner] object PlanInlineApply {
  private sealed abstract class ValueSource
  private case class DirectSource(storageLoc: StorageLocation, value: LocationValue) extends ValueSource
  private case class ClosureSource(storageLoc: StorageLocation) extends ValueSource

  private def planInline(enclosingState: PlannerState, applyState: PlannerState)(
      lambdaExpr: et.Lambda,
      args: List[(ContextLocated, iv.IntermediateValue)],
      schemeProcOpt: Option[iv.KnownSchemeProc] = None
  )(implicit plan: PlanWriter): PlanResult = {
    val procType = lambdaExpr.polyType.typeForArgs(args.map(_._2.schemeType))

    val closedVars = FindClosedVars(enclosingState, lambdaExpr, None)

    val valueSources = closedVars map {
      case ImportedImmutable(storageLoc, parentIntermediate) =>
        DirectSource(storageLoc, ImmutableValue(parentIntermediate))

      case commonCapture: CapturedVariable if applyState.values.contains(commonCapture.storageLoc) =>
        // This is captured variable the lambda expression and our inline state have in common
        // We can just import it directly for the purposes of inlining
        DirectSource(commonCapture.storageLoc, applyState.values(commonCapture.storageLoc))

      case closureCapture: CapturedVariable =>
        ClosureSource(closureCapture.storageLoc)
    }

    // These values we can use directly
    val directValues = valueSources collect { case DirectSource(storageLoc, value) =>
      storageLoc -> value
    }

    // These values we need to load from our closure
    val wantedClosureValues = valueSources.collect({ case ClosureSource(storageLoc) =>
      storageLoc
    }).toSet

    val closureValues = schemeProcOpt.flatMap { schemeProc =>
      schemeProc.selfTempOpt.map { selfTemp =>
        // Fill in the rest of our values from our closure
        LoadClosureData(selfTemp, schemeProc.manifest, Some(wantedClosureValues))
      }
    } getOrElse(Map())

    val postClosureState = PlannerState(
      values=(directValues ++ closureValues).toMap,
      // It's very important we get our parameter values from where we're inlined, not where we're defined
      parameterValues=applyState.parameterValues,
      inlineDepth=applyState.inlineDepth + 1
    )

    // Add our provided fixed arguments to the state
    val fixedArgLocs = lambdaExpr.mandatoryArgs ++ lambdaExpr.optionalArgs.map(_.storageLoc)
    val postFixedArgState = fixedArgLocs.zip(args).foldLeft(postClosureState) {
      case (state, (storageLoc, (_, argValue))) =>
        val castValue = argValue.castToSchemeType(storageLoc.schemeType)
        state.withValue(storageLoc -> InitLocationValue(storageLoc, castValue))
    }

    val defaultedOptionalCount = (fixedArgLocs.length - args.length)
    val postDefaultOptState = lambdaExpr.optionalArgs.takeRight(defaultedOptionalCount).foldLeft(postFixedArgState) {
      case (state, et.OptionalArg(storageLoc, defaultExpr)) =>
        val defaultResult = PlanExpr(state)(defaultExpr)
        val defaultValue = defaultResult.value

        // Note we don't use the state from the default value. The lambda planner doesn't do this as this code is
        // conditionally executed so it can't introduce bindings etc to the parent state
        val castValue = defaultValue.castToSchemeType(storageLoc.schemeType)
        state.withValue(storageLoc -> InitLocationValue(storageLoc, castValue))
    }

    val restArgImmutables = lambdaExpr.restArgOpt.zip(lambdaExpr.schemeType.restArgMemberTypeOpt) map {
      case (storageLoc, memberType) =>
        val restValues = args.drop(fixedArgLocs.length).map(_._2)

        val castValues = restValues.map(_.castToSchemeType(memberType))
        storageLoc -> InitLocationValue(storageLoc, ValuesToList(castValues))
    }

    val postRestArgState = postDefaultOptState.withValues(restArgImmutables.toMap)

    val postSelfState = schemeProcOpt.foldLeft(postRestArgState) { (state, selfValue) =>
      selfValue.manifest.recursiveSelfLocOpt.foldLeft(state) { (state, selfLoc) =>
        state.withValue(selfLoc -> InitLocationValue(selfLoc, selfValue))
      }
    }

    val planResult = PlanExpr(postSelfState)(lambdaExpr.body)

    // Make sure our return type is of the declared type
    val stableReturnType = vt.StabiliseReturnType(procType.returnType)
    val castValue = planResult.value.castToSchemeType(stableReturnType.schemeType)

    // Does the return value have a value constraint?
    // This is useful for predicate procedures like (zero?) to propagate information about their arguments.
    val finalApplystate = planResult.state.valueConstraintState.condActions.get(castValue) match {
      case Some(condActions) =>
        ConstrainValue.addCondActions(applyState)(castValue, condActions)

      case _ =>
        applyState
    }

    PlanResult(state=finalApplystate, value=castValue)
  }

  /** Inlines a self-executing lambda
    *
    * These are in the form of ((lambda (arg) x) val) and analogous forms. These are occur frequently as the result of
    * macro expansion of simpler forms.
    *
    * @param  state       State the SEL is executing in
    * @param  lambdaExpr  Lambda expression being self-executed
    * @param  args        Arguments for the SEL
    */
  def fromSEL(state: PlannerState)(
      lambdaExpr: et.Lambda,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): PlanResult =
    planInline(state, state)(lambdaExpr, args)

  /** Inlines an already planned lambda from its manifest
    *
    * @param  applyState   State the lambda in being applied in. This is used to avoid loading values from the
    *                      procedure's closure that also exist in the applying environment.
    * @param  schemeProc   Procedure value being inlined
    */
  def fromKnownSchemeProc(applyState: PlannerState)(
      schemeProc: iv.KnownSchemeProc,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): PlanResult =
    planInline(schemeProc.manifest.parentState, applyState)(
      lambdaExpr=schemeProc.manifest.lambdaExpr,
      args=args,
      schemeProcOpt=Some(schemeProc)
    )
}
