package io.llambda.compiler.planner
import io.llambda

import collection.immutable.ListSet
import collection.breakOut
import annotation.tailrec

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ProcedureSignature, ProcedureAttribute}
import llambda.compiler.ImpossibleTypeConversionException
import llambda.compiler.codegen.CompactRepresentationForType

private[planner] object PlanLambda {
  private sealed abstract class Argument {
    val storageLoc : StorageLocation
    val tempValue : ps.TempValue
    val valueType : vt.ValueType
  }

  private case class FixedArgument(
    storageLoc : StorageLocation,
    tempValue : ps.TempValue,
    valueType : vt.ValueType
  ) extends Argument
  
  private case class RestArgument(
    storageLoc : StorageLocation,
    tempValue : ps.TempValue
  ) extends Argument {
    val valueType = vt.ListElementType
  }

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

  private def initializeMutableArgs(initialState : PlannerState)(mutableArgs : List[Argument])(implicit plan : PlanWriter) : PlannerState = mutableArgs.length match {
    case 0 =>
      // Nothing to do
      initialState

    case argCount =>
      mutableArgs.foldLeft(initialState) { case (state, argument) =>
        val argValue = TempValueToIntermediate(argument.valueType, argument.tempValue)(plan.config)

        // Init the mutable
        val mutableTemp = ps.RecordTemp()
        val recordDataTemp = ps.RecordLikeDataTemp()

        // Determine our type and convert the argument to it
        val compactInnerType = CompactRepresentationForType(argument.valueType)
        val mutableType = MutableType(compactInnerType)
        val tempValue = argValue.toTempValue(compactInnerType)

        plan.steps += ps.InitRecordLike(mutableTemp, recordDataTemp, mutableType, isUndefined=false)

        // Set the value
        plan.steps += ps.SetRecordDataField(recordDataTemp, mutableType, mutableType.recordField, tempValue)
        
        state.withValue(argument.storageLoc -> MutableValue(mutableType, mutableTemp, false))
      }
  }

  private def loadClosureData(initialState : PlannerState)(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter) : PlannerState = 
    capturedVariables.foldLeft(initialState) { case (state, capturedVar) =>
      // Load the variable
      val varTemp = new ps.TempValue(capturedVar.recordField.fieldType.isGcManaged)
      plan.steps += ps.LoadRecordDataField(varTemp, closureDataTemp, closureType, capturedVar.recordField) 

      // Add it to our state
      capturedVar match {
        case immutable : CapturedImmutable =>
          val varValue = immutable.parentIntermediate.restoreFromClosure(capturedVar.valueType, varTemp)(plan.config)
          state.withValue(capturedVar.storageLoc -> ImmutableValue(varValue))

        case capturedMutable : CapturedMutable => 
          state.withValue(capturedVar.storageLoc -> capturedMutable.parentMutable.copy(mutableTemp=varTemp))
      }
    }
  
  private def storeClosureData(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter) : Unit = {
    for(capturedVar <- capturedVariables) {
      val varTemp = capturedVar match {
        case immutable : CapturedImmutable =>
          // Cast the value to its preferred type
          immutable.parentIntermediate.toTempValue(capturedVar.valueType, convertProcType=false)

        case mutable : CapturedMutable =>
          // Store the pointer to the mutable directly
          mutable.parentMutable.mutableTemp
      }
        
      // Store to the field
      plan.steps += ps.SetRecordDataField(closureDataTemp, closureType, capturedVar.recordField, varTemp)
    }
  }

  def apply(parentState : PlannerState, parentPlan : PlanWriter)(
      lambdaExpr : et.Lambda,
      sourceNameHint : Option[String],
      recursiveSelfLoc : Option[StorageLocation]
  ) : iv.KnownSchemeProc = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")
    val nativeSymbol = parentPlan.allocSymbol(sourceName)
      
    val fixedArgLocs = lambdaExpr.fixedArgs
    val restArgLoc = lambdaExpr.restArgOpt
    val body = lambdaExpr.body

    val closedVariables = FindClosedVars(parentState, body, recursiveSelfLoc)
    
    // Collect only the capture variables
    val capturedVariables = (closedVariables collect {
      case captured : CapturedVariable => captured
    }).toList

    // Determine if we have a closure
    val innerSelfTempOpt = if (capturedVariables.isEmpty) {
      None
    }
    else {
      Some(ps.CellTemp(ct.ProcedureCell))
    }

    // Make our closure type
    val closureType = if (innerSelfTempOpt.isDefined) {
      val closureSourceName = sourceName + "-closure"
      new vt.ClosureType(closureSourceName, capturedVariables.map(_.recordField))
    }
    else {
      vt.EmptyClosureType
    }
    
    // See if we can retype some of our args
    val argTypeMapping = RetypeLambdaArgs(lambdaExpr)(parentState, parentPlan.config)
    val retypedFixedArgs = fixedArgLocs.map({ argLoc =>
      val retypedType = argTypeMapping.getOrElse(argLoc, argLoc.schemeType)
      val compactType = CompactRepresentationForType(retypedType)

      (argLoc -> compactType)
    })

    // Build as list of all of our args
    val allArgs = retypedFixedArgs.map({ case (storageLoc, schemeType) =>
      FixedArgument(storageLoc, ps.Temp(schemeType), schemeType)
    }).toList ++
    restArgLoc.map({ storageLoc =>
      RestArgument(storageLoc, ps.CellTemp(ct.ListElementCell))
    })

    // Split our args in to mutable and immutable
    val (mutableArgs, immutableArgs) = allArgs.partition { argument =>
      parentPlan.config.analysis.mutableVars.contains(argument.storageLoc)
    }

    // Immutable vars can be used immediately
    val importedImmutables = (closedVariables.collect {
      case imported : ImportedImmutable => 
        (imported.storageLoc, ImmutableValue(imported.parentIntermediate))
    }).toMap

    val argImmutables = (immutableArgs.map {
      case FixedArgument(storageLoc, tempValue, valueType) =>
        (storageLoc, ImmutableValue(TempValueToIntermediate(valueType, tempValue)(parentPlan.config)))

      case RestArgument(storageLoc, tempValue) =>
        val restValue = new iv.CellValue(
          schemeType=storageLoc.schemeType,
          boxedValue=BoxedValue(ct.ListElementCell, tempValue)
        )

        (storageLoc, ImmutableValue(restValue))
    }).toMap

    // Prefer compact return types where possible
    val compactReturnType = lambdaExpr.schemeType.returnType match {
      case vt.ReturnType.SingleValue(schemeType) =>
        vt.ReturnType.SingleValue(CompactRepresentationForType(schemeType))

      case other =>
        other
    }

    // Determine our initial signature
    // This is fun - try renaming scalaBugSignature to initialSignature and remove the assignment below
    val scalaBugSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=innerSelfTempOpt.isDefined,
      restArgMemberTypeOpt=lambdaExpr.schemeType.restArgMemberTypeOpt,
      fixedArgTypes=retypedFixedArgs.map(_._2),
      returnType=compactReturnType,
      attributes=Set(ProcedureAttribute.FastCC)
    ) : ProcedureSignature

    val initialSignature : ProcedureSignature = scalaBugSignature

    // If we're recursive we can't deviate from our initial signature
    val canRefineSignature = !recursiveSelfLoc.isDefined

    // Create a value to reference ourselves recursively if required
    val recursiveSelfImmutables = recursiveSelfLoc map { storageLoc =>
      storageLoc -> ImmutableValue(
        new iv.KnownUserProc(
          signature=initialSignature,
          plannedSymbol=nativeSymbol,
          selfTempOpt=innerSelfTempOpt
        )
      )
    }

    // Start a new state for the procedure
    val initialImmutables = argImmutables ++ importedImmutables ++ recursiveSelfImmutables

    val preMutableState = PlannerState(
      values=initialImmutables,
      inlineDepth=parentState.inlineDepth
    )

    val procPlan = parentPlan.forkPlan() 

    // Initialize all of our mutable parameters
    val postMutableState = initializeMutableArgs(preMutableState)(mutableArgs)(procPlan)
    
    // Load all of our captured variables
    val postClosureState = innerSelfTempOpt match {
      case None =>
        postMutableState

      case Some(procSelf) => 
        val closureDataTemp = ps.RecordLikeDataTemp()
        procPlan.steps += ps.LoadRecordLikeData(closureDataTemp, procSelf, closureType)

        loadClosureData(postMutableState)(closureDataTemp, closureType, capturedVariables)(procPlan)
    }

    // Plan the body
    val planResult = PlanExpr(postClosureState)(body)(procPlan)

    val returnType = if (!canRefineSignature || ContainsImmediateReturn(body)) {
      // Return an arbitrary number of values with arbitrary types
      // XXX: We can be more clever here and try to find a common return type across all returns
      initialSignature.returnType
    }
    else {
      val resultReturnType = planResult.values.preferredReturnType
      val declaredReturnType = initialSignature.returnType

      // This is essentially intersecting the return types
      if (vt.ConvertibleToReturnType(declaredReturnType, resultReturnType) == Some(true)) {
        resultReturnType
      }
      else if (vt.ConvertibleToReturnType(resultReturnType, declaredReturnType) == Some(true)) {
        declaredReturnType
      }
      else {
        throw new ImpossibleTypeConversionException(
          parentPlan.activeContextLocated,
          s"Result return type of ${resultReturnType} cannot be converted to declared return type of ${declaredReturnType}"
        )
      }
    }

    val lastExprOpt = lastNonStructuralExpr(body)

    // Return from the function
    procPlan.withContextLocationOpt(lastExprOpt) {
      val resultTempOpt = planResult.values.toReturnTempValue(returnType)(procPlan)
      procPlan.steps += ps.Return(resultTempOpt)
    }
    
    val steps = procPlan.steps.toList

    val (worldPtrOpt, procSignature) = if (canRefineSignature && !WorldPtrUsedBySteps(steps)) {
      // World pointer is not required, strip it out
      (None, initialSignature.copy(hasWorldArg=false, returnType=returnType))
    }
    else {
      (Some(ps.WorldPtrValue), initialSignature.copy(returnType=returnType))
    }

    val argumentUniquer = new SourceNameUniquer

    // Name our function arguments
    val namedArguments =
      worldPtrOpt.toList.map({ worldPtr =>
        ("world" -> worldPtr)
      }) ++
      innerSelfTempOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      (allArgs.map { argument =>
        (argumentUniquer(argument.storageLoc.sourceName) -> argument.tempValue)
      })

    val irCommentOpt =
      for(location <- lambdaExpr.locationOpt)
      yield
        s"Scheme procedure defined at ${location.locationOnlyString}"

    // Determine our procedure
    val plannedFunction = PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=steps,
      debugContextOpt=lambdaExpr.debugContextOpt,
      irCommentOpt=irCommentOpt
    )

    val outerSelfTempOpt = innerSelfTempOpt map { _ => 
      // Save the closure values from the parent's scope
      val cellTemp = ps.RecordTemp()
      val dataTemp = ps.RecordLikeDataTemp()

      parentPlan.steps += ps.InitRecordLike(cellTemp, dataTemp, closureType, isUndefined=false)

      // Create our closure
      storeClosureData(dataTemp, closureType, capturedVariables)(parentPlan)

      // Store our entry point
      val entryPointTemp = ps.EntryPointTemp()
      parentPlan.steps += ps.CreateNamedEntryPoint(entryPointTemp, procSignature, nativeSymbol)
      parentPlan.steps += ps.SetProcedureEntryPoint(cellTemp, entryPointTemp)

      cellTemp
    }

    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction) 

    new iv.KnownSchemeProc(
      signature=procSignature,
      plannedSymbol=nativeSymbol,
      parentState=parentState,
      lambdaExpr=lambdaExpr,
      selfTempOpt=outerSelfTempOpt,
      recursiveSelfLoc=recursiveSelfLoc
    )
  }
}

