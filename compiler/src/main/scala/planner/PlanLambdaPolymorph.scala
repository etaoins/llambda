package io.llambda.compiler.planner
import io.llambda

import annotation.tailrec

import llambda.compiler.et
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{StorageLocation, ProcedureSignature, ProcedureAttribute}
import llambda.compiler.codegen.CompactRepresentationForType
import llambda.compiler.TypeException

object PlanLambdaPolymorph {
  private abstract class Argument {
    val name : String
    val tempValue : ps.TempValue
  }

  private case class MandatoryArgument(
      storageLoc : StorageLocation,
      tempValue : ps.TempValue,
      valueType : vt.ValueType
  ) extends Argument {
    val name = storageLoc.sourceName
  }

  private case class VarArgument(name : String, tempValue : ps.TempValue) extends Argument

  private case class RetypedOptional(
      storageLoc : StorageLocation,
      schemeType : vt.SchemeType,
      defaultExpr : et.Expr
  )

  /** Finds the last expression in a body that corresponds a user-produced expression
    *
    * This is used to source locate our return value-related steps
    */
  @tailrec
  private def lastNonStructuralExpr(expr : et.Expr) : Option[et.Expr] = expr match {
    case et.Begin(Nil) =>
      None

    case et.Begin(subexprs) =>
      lastNonStructuralExpr(subexprs.last)

    case et.InternalDefine(_, bodyExpr) =>
      lastNonStructuralExpr(bodyExpr)

    case other =>
      Some(other)
  }

  private def initLocationValue(
      storageLoc : StorageLocation,
      value : iv.IntermediateValue
  )(implicit plan : PlanWriter) : LocationValue = {
    if (plan.config.analysis.mutableVars.contains(storageLoc)) {
      // Init the mutable
      val mutableTemp = ps.RecordTemp()

      // Determine our type and convert the argument to it
      val compactInnerType = CompactRepresentationForType(value.schemeType)
      val mutableType = MutableType(compactInnerType)
      val tempValue = value.toTempValue(compactInnerType)

      // Set the value
      val fieldValues = Map[vt.RecordField, ps.TempValue](mutableType.recordField -> tempValue)
      plan.steps += ps.InitRecord(mutableTemp, mutableType, fieldValues, isUndefined=false)

      MutableValue(mutableType, mutableTemp, false)
    }
    else {
      ImmutableValue(value)
    }
  }

  private def resolveOptionalArgs(state : PlannerState)(
      varArgsListHead : iv.IntermediateValue,
      arg : RetypedOptional
  )(implicit plan : PlanWriter) : (iv.IntermediateValue, PlannerState) = arg match {
    case RetypedOptional(storageLoc, schemeType, defaultExpr) =>
      val varArgsListHeadTemp = varArgsListHead.toBoxedValue().tempValue
      val isPairPred = ps.Temp(vt.Predicate)
      plan.steps += ps.TestCellType(isPairPred, varArgsListHeadTemp, ct.PairCell, Set(ct.PairCell, ct.EmptyListCell))

      val providedValuePlan = plan.forkPlan()
      val providedValuePair = varArgsListHead.withSchemeType(varArgsListHead.schemeType & vt.AnyPairType)

      val providedValue = PlanCadr.loadCar(providedValuePair)(providedValuePlan)
      val providedListHead = PlanCadr.loadCdr(providedValuePair)(providedValuePlan)


      val defaultValuePlan = plan.forkPlan()
      val defaultValueResult = PlanExpr(state)(defaultExpr)(defaultValuePlan)

      val defaultValue = defaultValuePlan.withContextLocation(defaultExpr) {
        val uncastDefaultValue = defaultValueResult.value
        uncastDefaultValue.castToSchemeType(storageLoc.schemeType)(defaultValuePlan)
      }


      val valuePhiResult = PlanValuePhi(
        leftPlan=providedValuePlan,
        leftValue=providedValue,
        rightPlan=defaultValuePlan,
        rightValue=defaultValue
      )

      val listHeadPhiResult = PlanValuePhi(
        leftPlan=providedValuePlan,
        leftValue=providedListHead,
        rightPlan=defaultValuePlan,
        rightValue=iv.EmptyListValue
      )

      val valuePhis = List(
        ps.ValuePhi(valuePhiResult.resultTemp, valuePhiResult.leftTempValue, valuePhiResult.rightTempValue),
        ps.ValuePhi(listHeadPhiResult.resultTemp, listHeadPhiResult.leftTempValue, listHeadPhiResult.rightTempValue)
      )

      plan.steps += ps.CondBranch(isPairPred, providedValuePlan.steps.toList, defaultValuePlan.steps.toList, valuePhis)

      val argValue = valuePhiResult.resultValue
      val stateWithOptionalArg = state.withValue(storageLoc -> initLocationValue(storageLoc, argValue))

      (listHeadPhiResult.resultValue, stateWithOptionalArg)
  }

  private def retypeFixedArg(
      argLoc : StorageLocation,
      retypeMapping : Map[StorageLocation, vt.SchemeType],
      declType : vt.SchemeType
  ) : vt.SchemeType = retypeMapping.get(argLoc) match {
    case Some(discoveredType) => discoveredType & declType
    case None => declType
  }

  def apply(
      nativeSymbol : String,
      manifest : LambdaManifest,
      polymorphType : vt.ProcedureType,
      isPrimaryPolymorph : Boolean
  )(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val lambdaExpr = manifest.lambdaExpr
    val body = lambdaExpr.body

    val parentState = manifest.parentState

    val optionalArgLocs = lambdaExpr.optionalArgs.map(_.storageLoc)

    // Determine if we have a closure
    val innerSelfTempOpt = if (manifest.capturedVars.isEmpty) {
      None
    }
    else {
      Some(ps.CellTemp(ct.ProcedureCell))
    }

    // See if we can retype some of our args
    val argTypeMapping = RetypeLambdaArgs(lambdaExpr, polymorphType)(parentState, parentPlan.config)

    val retypedMandatoryArgs = lambdaExpr.mandatoryArgs.zip(polymorphType.mandatoryArgTypes) map {
      case (argLoc, declType) =>
        val schemeType = retypeFixedArg(argLoc, argTypeMapping, declType)

        (argLoc -> CompactRepresentationForType(schemeType))
    }

    val retypedOptionalArgs = lambdaExpr.optionalArgs.zip(polymorphType.optionalArgTypes) map {
      case (et.OptionalArg(argLoc, defaultExpr), declType) =>
        RetypedOptional(argLoc, retypeFixedArg(argLoc, argTypeMapping, declType), defaultExpr)
    }

    // Prefer compact return types where possible
    val compactReturnType = polymorphType.returnType match {
      case vt.ReturnType.Reachable(schemeType) =>
        vt.ReturnType.Reachable(CompactRepresentationForType(schemeType))

      case other =>
        other
    }

    // Determine our initial signature
    val initialSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=innerSelfTempOpt.isDefined,
      mandatoryArgTypes=retypedMandatoryArgs.map(_._2),
      optionalArgTypes=retypedOptionalArgs.map(_.schemeType),
      restArgMemberTypeOpt=polymorphType.restArgMemberTypeOpt,
      returnType=compactReturnType,
      attributes=Set(ProcedureAttribute.FastCC)
    ) : ProcedureSignature

    // If we're recursive we can't deviate from our initial signature
    val canRefineSignature = !manifest.isRecursive

    // Initialise our plan
    val procPlan = parentPlan.forkPlan()
    val initialState = PlannerState(values=Map(), inlineDepth=parentState.inlineDepth)

    val importedImmutablesState = manifest.closedVars.foldLeft(initialState)({
      case (state, imported : ImportedImmutable) =>
        state.withValue(imported.storageLoc -> ImmutableValue(imported.parentIntermediate))

      case (state, _) =>
        state
    })

    // Load all of our captured variables
    val postClosureState = innerSelfTempOpt match {
      case None =>
        importedImmutablesState

      case Some(innerSelfTemp) =>
        val primarySelfTemp = if (isPrimaryPolymorph) {
          // We're the primary polymorph - use our closure directly
          innerSelfTemp
        }
        else {
          // Load the closure of the primary polymorph
          val closureDataTemp = ps.RecordLikeDataTemp()
          procPlan.steps += ps.LoadRecordLikeData(closureDataTemp, innerSelfTemp, AdapterProcType)

          val primarySelfTemp = ps.CellTemp(ct.ProcedureCell)
          procPlan.steps += ps.LoadRecordDataField(primarySelfTemp, closureDataTemp, AdapterProcType, AdapterProcField)

          primarySelfTemp
        }

        val closureValues = LoadClosureData(primarySelfTemp, manifest)(procPlan)
        importedImmutablesState.withValues(closureValues)
    }

    // Import all of our mandatory args
    val mandatoryArgs = retypedMandatoryArgs.map({ case (storageLoc, valueType) =>
      MandatoryArgument(storageLoc, ps.Temp(valueType), valueType)
    })

    val postMandatoryState = mandatoryArgs.foldLeft(postClosureState) {
      case (state, MandatoryArgument(storageLoc, tempValue, valueType)) =>
        val value = TempValueToIntermediate(valueType, tempValue)
        state.withValue(storageLoc -> initLocationValue(storageLoc, value)(procPlan))
    }

    // Now the optional args
    val varArgTemp = ps.CellTemp(ct.ListElementCell)
    val varArgType = vt.VariableArgsToListType(
      optionalArgs=retypedOptionalArgs.map(_.schemeType),
      restArgMemberTypeOpt=polymorphType.restArgMemberTypeOpt
    )

    val varArgValue = new iv.CellValue(
      schemeType=varArgType,
      boxedValue=BoxedValue(ct.ListElementCell, varArgTemp)
    ) : iv.IntermediateValue

    val (restValue, postOptionalState) = retypedOptionalArgs.foldLeft((varArgValue, postMandatoryState)) {
      case ((varArgValue, state), arg) =>
        resolveOptionalArgs(state)(varArgValue, arg)(procPlan)
    }

    // And the rest argument
    val postRestState = lambdaExpr.restArgOpt.foldLeft(postOptionalState) { (state, restArgLoc) =>
      state.withValue(restArgLoc -> initLocationValue(restArgLoc, restValue)(procPlan))
    }

    // Finally, the self argument
    val postSelfState = manifest.recursiveSelfLocOpt.foldLeft(postRestState) { (state, selfLoc) =>
      val selfValue = new iv.KnownUserProc(
        polySignature=initialSignature.toPolymorphic,
        plannedSymbol=nativeSymbol,
        selfTempOpt=innerSelfTempOpt
      )

      state.withValue(selfLoc -> initLocationValue(selfLoc, selfValue)(procPlan))
    }

    // Plan the body
    val planResult = PlanExpr(postSelfState)(body)(procPlan)

    val finalReturnType = if (!canRefineSignature) {
      // Return an arbitrary number of values with arbitrary types
      initialSignature.returnType
    }
    else {
      val resultReturnType = planResult.value.preferredReturnType
      val declaredReturnType = initialSignature.returnType

      // This is essentially intersecting the return types
      if (vt.ConvertibleToReturnType(declaredReturnType, resultReturnType) == Some(true)) {
        resultReturnType
      }
      else if (vt.ConvertibleToReturnType(resultReturnType, declaredReturnType) == Some(true)) {
        declaredReturnType
      }
      else {
        throw new TypeException(
          parentPlan.activeContextLocated,
          s"Result return type of ${resultReturnType} cannot be converted to declared return type of ${declaredReturnType}"
        )
      }
    }

    val lastExprOpt = lastNonStructuralExpr(body)

    // Return from the function
    procPlan.withContextLocationOpt(lastExprOpt) {
      val resultTempOpt = planResult.value.toReturnTempValueOpt(finalReturnType)(procPlan)
      procPlan.steps += ps.Return(resultTempOpt)
    }

    val steps = procPlan.steps.toList

    val (worldPtrOpt, strippedWorldPtrSignature) = if (canRefineSignature && !WorldPtrUsedBySteps(steps)) {
      // World pointer is not required, strip it out
      (None, initialSignature.copy(hasWorldArg=false, returnType=finalReturnType))
    }
    else {
      (Some(ps.WorldPtrValue), initialSignature.copy(returnType=finalReturnType))
    }

    val procSignature = if (planResult.value == iv.UnreachableValue) {
      strippedWorldPtrSignature.copy(
        attributes=strippedWorldPtrSignature.attributes + ProcedureAttribute.NoReturn
      )
    }
    else {
      strippedWorldPtrSignature
    }

    val argumentUniquer = new SourceNameUniquer
    argumentUniquer.reserve("world", "self")

    // Make a useful name for our variable argument
    val varArgOpt = (lambdaExpr.optionalArgs, lambdaExpr.restArgOpt) match {
      case (Nil, None) => None
      case (Nil, Some(restOnlyLoc)) => Some(VarArgument(restOnlyLoc.sourceName, varArgTemp))
      case (_, None) => Some(VarArgument("optionals", varArgTemp))
      case _ => Some(VarArgument("varArgs", varArgTemp))
    }

    val allArgs = mandatoryArgs ++ varArgOpt

    // Name our function arguments
    val namedArguments =
      worldPtrOpt.toList.map({ worldPtr =>
        ("world" -> worldPtr)
      }) ++
      innerSelfTempOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      (allArgs.map { argument =>
        (argumentUniquer(argument.name) -> argument.tempValue)
      })

    val irCommentOpt =
      for(location <- lambdaExpr.locationOpt)
      yield
        s"Scheme procedure defined at ${location.locationOnlyString}"

    PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=steps,
      debugContextOpt=lambdaExpr.debugContextOpt,
      irCommentOpt=irCommentOpt
    )
  }
}
