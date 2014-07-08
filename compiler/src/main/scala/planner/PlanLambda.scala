package io.llambda.compiler.planner
import io.llambda

import collection.immutable.ListSet
import collection.breakOut
import annotation.tailrec

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ProcedureSignature}
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

  private sealed abstract class ClosedVariable

  private case class ImportedImmutable(
    storageLoc : StorageLocation,
    parentIntermediate : iv.IntermediateValue
  ) extends ClosedVariable

  private sealed abstract class CapturedVariable extends ClosedVariable {
    val storageLoc : StorageLocation
    val valueType : vt.ValueType
    val recordField : vt.RecordField
  }

  private case class CapturedImmutable(
    storageLoc : StorageLocation,
    parentIntermediate : iv.IntermediateValue,
    valueType : vt.ValueType,
    recordField : vt.RecordField
  ) extends CapturedVariable
  
  private case class CapturedMutable(
    storageLoc : StorageLocation,
    parentMutable : MutableValue,
    recordField : vt.RecordField
  ) extends CapturedVariable {
    val valueType = parentMutable.mutableType
  }

  /** Finds all referenced variables in an expression and returns them in a stable order */
  private def findRefedVariables(expr : et.Expr) : List[StorageLocation] = expr match {
    case et.VarRef(variable) =>
      List(variable)

    case et.MutateVar(variable, expr) =>
      variable :: findRefedVariables(expr)

    case otherExpr =>
      otherExpr.subexprs.flatMap(findRefedVariables)
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

  private def containsImmediateReturn(expr : et.Expr) : Boolean = expr match {
    case _ : et.Return =>
      true

    case lambda : et.Lambda  =>
      // If the return exists in tbe body of a nested lambda it's not immediate
      false

    case _ =>
      expr.subexprs.exists(containsImmediateReturn)
  }

  def apply(parentState : PlannerState, parentPlan : PlanWriter)(
      lambdaExpr : et.Lambda,
      sourceNameHint : Option[String],
      recursiveSelfLoc : Option[StorageLocation]
  )(implicit planConfig : PlanConfig) : PlanResult = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")
    val nativeSymbol = parentPlan.allocProcedureSymbol(sourceName)
      
    val fixedArgLocs = lambdaExpr.fixedArgs
    val restArgLoc = lambdaExpr.restArg
    val body = lambdaExpr.body

    // Find the variables that are closed by the parent scope
    val refedVarsList = findRefedVariables(body)

    // Figure out if the immutables need to be captured
    val closedVariables = (refedVarsList.distinct flatMap { storageLoc =>
      parentState.values.get(storageLoc) map {
        case ImmutableValue(parentIntermediate) =>
          if (parentIntermediate.needsClosureRepresentation) {
            val compactType = CompactRepresentationForType(parentIntermediate.preferredRepresentation)
            val recordField = new vt.RecordField(storageLoc.sourceName, compactType)

            // We have to capture this
            CapturedImmutable(storageLoc, parentIntermediate, compactType, recordField)
          }
          else {
            // No need for capturing - import the intermediate value directly
            ImportedImmutable(storageLoc, parentIntermediate)
          }

      case parentMutable : MutableValue =>
        val recordField = new vt.RecordField(storageLoc.sourceName, parentMutable.mutableType)
        CapturedMutable(storageLoc, parentMutable, recordField)
      }
    })
    
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
    val argTypeMapping = RetypeLambdaArgs(lambdaExpr)(parentState, planConfig)
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
      planConfig.analysis.mutableVars.contains(argument.storageLoc)
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
          schemeType=vt.ListElementType,
          tempType=ct.ListElementCell,
          tempValue=tempValue,
          properListCell=true // Our ABI guarantees that this is a proper list
        )

        (storageLoc, ImmutableValue(restValue))
    }).toMap 

    // Determine our initial signature
    val initialSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=innerSelfTempOpt.isDefined,
      hasRestArg=restArgLoc.isDefined,
      fixedArgs=retypedFixedArgs.map(_._2),
      returnType=Some(vt.AnySchemeType),
      attributes=Set()
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
    val planResult = PlanExpr(postClosureState)(body)(planConfig, procPlan)

    val procSignature = if (!canRefineSignature) {
      initialSignature
    }
    else {
      val returnTypeOpt = if (containsImmediateReturn(body)) {
        // Return a DatumCell
        // XXX: We can be more clever here and try to find a common return type across all returns
        Some(vt.AnySchemeType)
      }
      else if (planResult.value.hasDefiniteCellType(ct.UnitCell)) {
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
      procPlan.steps += ps.Return(procSignature.returnType map { returnType =>
        planResult.value.toTempValue(returnType)(procPlan, worldPtr)
      })
    }
    
    // Name our function arguments
    val namedArguments =
      ("world" -> worldPtr) ::
      innerSelfTempOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      (allArgs.map { argument =>
        (argument.storageLoc.sourceName -> argument.tempValue)
      })

    val steps = procPlan.steps.toList
    val worldPtrRequired = WorldPtrUsedBySteps(steps, worldPtr)

    // Determine our procedure
    val plannedFunction = PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=steps,
      worldPtrOpt=if (worldPtrRequired) Some(worldPtr) else None,
      debugContextOpt=lambdaExpr.debugContextOpt
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

    val procValue = new iv.KnownUserProc(
      signature=plannedFunction.signature,
      plannedSymbol=nativeSymbol,
      selfTempOpt=outerSelfTempOpt
    )

    PlanResult(
      state=parentState,
      value=procValue
    )
  }
}

