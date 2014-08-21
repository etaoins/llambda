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

  private def initializeMutableArgs(initialState : PlannerState)(mutableArgs : List[Argument])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : PlannerState = mutableArgs.length match {
    case 0 =>
      // Nothing to do
      initialState

    case argCount =>
      mutableArgs.foldLeft(initialState) { case (state, argument) =>
        val argValue = TempValueToIntermediate(argument.valueType, argument.tempValue)

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
          val varValue = immutable.parentIntermediate.restoreFromClosure(capturedVar.valueType, varTemp)
          state.withValue(capturedVar.storageLoc -> ImmutableValue(varValue))

        case capturedMutable : CapturedMutable => 
          state.withValue(capturedVar.storageLoc -> capturedMutable.parentMutable.copy(mutableTemp=varTemp))
      }
    }
  
  private def storeClosureData(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Unit = {
    for(capturedVar <- capturedVariables) {
      val varTemp = capturedVar match {
        case immutable : CapturedImmutable =>
          // Cast the value to its preferred type
          immutable.parentIntermediate.toTempValue(capturedVar.valueType)

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
  ) : PlanResult = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")
    val nativeSymbol = parentPlan.allocProcedureSymbol(sourceName)
      
    val fixedArgLocs = lambdaExpr.fixedArgs
    val restArgLoc = lambdaExpr.restArgOpt.map(_.storageLoc)
    val body = lambdaExpr.body

    val closedVariables = FindClosedVars(parentState, body)
    
    // Collect only the capture variables
    val capturedVariables = (closedVariables collect {
      case captured : CapturedVariable => captured
    }).toList

    // Make a temp for the world pointer
    // Don't implicit this because we also need our parent's world ptr for storing closure data
    val worldPtr = new ps.WorldPtrValue()

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
      (argLoc -> argTypeMapping.getOrElse(argLoc, argLoc.schemeType))
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
        (storageLoc, ImmutableValue(TempValueToIntermediate(valueType, tempValue)))

      case RestArgument(storageLoc, tempValue) =>
        val restValue = new iv.CellValue(
          schemeType=storageLoc.schemeType,
          boxedValue=BoxedValue(ct.ListElementCell, tempValue)
        )

        (storageLoc, ImmutableValue(restValue))
    }).toMap 

    // Determine our initial signature
    val initialSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=innerSelfTempOpt.isDefined,
      restArgOpt=lambdaExpr.restArgOpt.map(_.memberType),
      fixedArgs=retypedFixedArgs.map(_._2),
      returnType=Some(vt.AnySchemeType),
      attributes=Set(ProcedureAttribute.FastCC)
    )

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
      worldPtr=worldPtr
    )

    val procPlan = parentPlan.forkPlan() 

    // Initialize all of our mutable parameters
    val postMutableState = initializeMutableArgs(preMutableState)(mutableArgs)(procPlan, worldPtr) 
    
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

    val retTypedSignature = if (!canRefineSignature) {
      initialSignature
    }
    else {
      val returnTypeOpt = if (ContainsImmediateReturn(body)) {
        // Return an AnyCell
        // XXX: We can be more clever here and try to find a common return type across all returns
        Some(vt.AnySchemeType)
      }
      else if (planResult.value.hasDefiniteType(vt.UnitType)) {
        // Instead of returning a unit cell just return void
        None
      }
      else {
        // Find the preferred representation of our return value
        Some(planResult.value.preferredRepresentation)
      }

      initialSignature.copy(returnType=returnTypeOpt)
    }

    val lastExprOpt = lastNonStructuralExpr(body)

    // Return from the function
    procPlan.withContextLocationOpt(lastExprOpt) {
      procPlan.steps += ps.Return(retTypedSignature.returnType map { returnType =>
        planResult.value.toTempValue(returnType)(procPlan, worldPtr)
      })
    }
    
    val steps = procPlan.steps.toList

    val (worldPtrOpt, procSignature) = if (canRefineSignature && !WorldPtrUsedBySteps(steps, worldPtr)) {
      // World pointer is not required, strip it out
      (None, retTypedSignature.copy(hasWorldArg=false))
    }
    else {
      (Some(worldPtr), retTypedSignature)
    }

    // Name our function arguments
    val namedArguments =
      worldPtrOpt.toList.map({ worldPtr =>
        ("world" -> worldPtr)
      }) ++
      innerSelfTempOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      (allArgs.map { argument =>
        (argument.storageLoc.sourceName -> argument.tempValue)
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
      worldPtrOpt=worldPtrOpt,
      debugContextOpt=lambdaExpr.debugContextOpt,
      irCommentOpt=irCommentOpt
    )

    val outerSelfTempOpt = innerSelfTempOpt map { _ => 
      // Save the closure values from the parent's scope
      val cellTemp = ps.RecordTemp()
      val dataTemp = ps.RecordLikeDataTemp()

      parentPlan.steps += ps.InitRecordLike(cellTemp, dataTemp, closureType, isUndefined=false)

      storeClosureData(dataTemp, closureType, capturedVariables)(parentPlan, parentState.worldPtr)

      cellTemp
    }

    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction) 

    val procValue = new iv.KnownSchemeProc(
      signature=plannedFunction.signature,
      plannedSymbol=nativeSymbol,
      parentState=parentState,
      lambdaExpr=lambdaExpr,
      selfTempOpt=outerSelfTempOpt,
      recursiveSelfLoc=recursiveSelfLoc
    )

    PlanResult(
      state=parentState,
      value=procValue
    )
  }
}

