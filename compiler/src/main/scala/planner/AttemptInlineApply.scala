package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ContextLocated}
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}

private[planner] object AttemptInlineApply {
  private sealed abstract class ValueSource
  private case class DirectSource(storageLoc: StorageLocation, value: LocationValue) extends ValueSource
  private case class ClosureSource(storageLoc: StorageLocation) extends ValueSource

  private def attemptInline(parentState: PlannerState, inlineState: PlannerState)(
      lambdaExpr: et.Lambda,
      args: List[(ContextLocated, iv.IntermediateValue)],
      selfTempOpt: Option[ps.TempValue] = None,
      manifestOpt: Option[LambdaManifest] = None
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    val mutableVars = plan.config.analysis.mutableVars
    val allArgs = lambdaExpr.mandatoryArgs ++ lambdaExpr.optionalArgs.map(_.storageLoc) ++ lambdaExpr.restArgOpt

    if (!(mutableVars & allArgs.toSet).isEmpty) {
      // Not supported yet
      return None
    }

    if (!HasCompatibleArity(
        args.length,
        lambdaExpr.mandatoryArgs.length,
        lambdaExpr.optionalArgs.length,
        lambdaExpr.restArgOpt.isDefined)) {
      // Incompatible arity - let PlanInvokeApply fail this
      return None
    }

    val procType = lambdaExpr.polyType.typeForArgs(args.map(_._2.schemeType))

    val closedVars = FindClosedVars(parentState, lambdaExpr, None)

    val valueSources = closedVars map {
      case ImportedImmutable(storageLoc, parentIntermediate) =>
        DirectSource(storageLoc, ImmutableValue(parentIntermediate))

      case commonCapture: CapturedVariable if inlineState.values.contains(commonCapture.storageLoc) =>
        // This is captured variable the lambda expression and our inline state have in common
        // We can just import it directly for the purposes of inlining
        DirectSource(commonCapture.storageLoc, inlineState.values(commonCapture.storageLoc))

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

    val closureValues = (selfTempOpt, manifestOpt) match {
      case (Some(selfTemp), Some(manifest)) =>
        // Fill in the rest of our values from our closure
        LoadClosureData(selfTemp, manifest, Some(wantedClosureValues))

      case _ =>
        if (wantedClosureValues.isEmpty) {
          Map()
        }
        else {
          // We need our closure but we don't have one
          return None
        }
    }

    val postClosureState = PlannerState(
      values=(directValues ++ closureValues).toMap,
      inlineDepth=inlineState.inlineDepth + 1
    )

    // Add our provided fixed arguments to the state
    val fixedArgLocs = lambdaExpr.mandatoryArgs ++ lambdaExpr.optionalArgs.map(_.storageLoc)
    val postFixedArgState = fixedArgLocs.zip(args).foldLeft(postClosureState) {
      case (state, (storageLoc, (_, argValue))) =>
        if (vt.SatisfiesType(storageLoc.schemeType, argValue.schemeType) != Some(true)) {
          // This type cast could fail at runtime
          return None
        }

        state.withValue(storageLoc -> ImmutableValue(argValue))
    }

    val defaultedOptionalCount = (fixedArgLocs.length - args.length)
    val postDefaultOptState = lambdaExpr.optionalArgs.takeRight(defaultedOptionalCount).foldLeft(postFixedArgState) {
      case (state, et.OptionalArg(storageLoc, defaultExpr)) =>
        val defaultResult = PlanExpr(state)(defaultExpr)
        val defaultValue = defaultResult.value

        if (vt.SatisfiesType(storageLoc.schemeType, defaultValue.schemeType) != Some(true)) {
          return None
        }

        // Note we don't use the state from the default value. The lambda planner doesn't do this as this code is
        // conditionally executed so it can't introduce bindings etc to the parent state
        state.withValue(storageLoc -> ImmutableValue(defaultValue))
    }


    val restArgImmutables = lambdaExpr.restArgOpt.zip(lambdaExpr.schemeType.restArgMemberTypeOpt) map {
      case (storageLoc, memberType) =>
        val restValues = args.drop(fixedArgLocs.length).map(_._2)

        for (restValue <- restValues)  {
          if (vt.SatisfiesType(memberType, restValue.schemeType) != Some(true)) {
            // This type cast could fail at runtime
            return None
          }
        }

        storageLoc -> ImmutableValue(ValuesToList(restValues))
    }

    val postRestArgState = postDefaultOptState.withValues(restArgImmutables.toMap)
    val planResult = PlanExpr(postRestArgState)(lambdaExpr.body)

    // Make sure our return type is of the declared type
    val stableReturnType = vt.StabiliseReturnType(procType.returnType)
    val castValues = planResult.value.castToSchemeType(stableReturnType.schemeType)

    Some(castValues)
  }

  /** Attempts to inline a self-executing lambda
    *
    * These are in the form of ((lambda (arg) x) val) and analogous forms. These are occur frequently as the result of
    * macro expansion of simpler forms.
    *
    * @param  state       State the SEL is executing in
    * @param  lambdaExpr  Lambda expression being self-executed
    * @param  args        Arguments for the SEL
    * @return Result values if inlining was successful, None otherwise
    */
  def fromSEL(state: PlannerState)(
      lambdaExpr: et.Lambda,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] =
    attemptInline(state, state)(lambdaExpr, args)

  /** Attempts to inline an already planned lambda from its manifest
    *
    * @param  inlineState  State the lambda in being inlined in to. This is used to avoid loading values from the
    *                      procedure's closure that also exist in the inlining environment.
    * @param  manifest     Manifest for the planned procedure
    * @param  selfTempOpt  Self value for the inlining procedure if it captured variables. This is used to access the
    *                      closure for the procedure.
    * @return Result values if inlining was successful, None otherwise
    */
  def fromManifiest(inlineState: PlannerState)(
      manifest: LambdaManifest,
      args: List[(ContextLocated, iv.IntermediateValue)],
      selfTempOpt: Option[ps.TempValue] = None
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] =
    attemptInline(manifest.parentState, inlineState)(
      lambdaExpr=manifest.lambdaExpr,
      args=args,
      selfTempOpt=selfTempOpt,
      manifestOpt=Some(manifest)
    )
}
