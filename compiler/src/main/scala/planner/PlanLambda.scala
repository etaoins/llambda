package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ProcedureSignature}

private[planner] object PlanLambda {
  private case class Argument(
    storageLoc : StorageLocation,
    tempValue : ps.TempValue,
    valueType : vt.ValueType
  )

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
    tempValue : ps.TempValue,
    recordField : vt.RecordField,
    needsUndefCheck : Boolean
  ) extends CapturedVariable {
    val valueType = vt.MutableType
  }

  private def findRefedVariables(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(variable) =>
      Set(variable)

    case et.MutateVar(variable, expr) =>
      Set(variable) ++ findRefedVariables(expr)

    case otherExpr =>
      otherExpr.subexpressions.flatMap(findRefedVariables).toSet
  }

  private def initializeMutableArgs(initialState : PlannerState)(mutableArgs : List[Argument])(implicit plan : PlanWriter) : PlannerState = mutableArgs.length match {
    case 0 =>
      // Nothing to do
      initialState

    case argCount =>
      mutableArgs.foldLeft(initialState) { case (state, Argument(storageLoc, tempValue, _)) =>
        // Init the mutable
        val mutableTemp = ps.GcManagedValue()
        val recordDataTemp = ps.GcUnmanagedValue()

        plan.steps += ps.RecordLikeInit(mutableTemp, recordDataTemp, vt.MutableType)

        // Set the value
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, tempValue)
        
        state.withValue(storageLoc -> MutableValue(mutableTemp, false))
      }
  }

  private def loadClosureData(initialState : PlannerState)(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter) : PlannerState = 
    capturedVariables.foldLeft(initialState) { case (state, capturedVar) =>
      // Load the variable
      val varTemp = new ps.TempValue(capturedVar.recordField.fieldType.isGcManaged)
      plan.steps += ps.RecordDataFieldRef(varTemp, closureDataTemp, closureType, capturedVar.recordField) 

      // Add it to our state
      capturedVar match {
        case _ : CapturedImmutable =>
          val varValue = TempValueToIntermediate(capturedVar.valueType, varTemp) 
          state.withValue(capturedVar.storageLoc -> ImmutableValue(varValue))

        case capturedMutable : CapturedMutable => 
          state.withValue(capturedVar.storageLoc -> MutableValue(varTemp, capturedMutable.needsUndefCheck))
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
          mutable.tempValue
      }
        
      // Store to the field
      plan.steps += ps.RecordDataFieldSet(closureDataTemp, closureType, capturedVar.recordField, varTemp)
    }
  }

  private def containsImmediateReturn(expr : et.Expression) : Boolean = expr match {
    case _ : et.Return =>
      true

    case lambda : et.Lambda  =>
      // If the return exists in tbe body of a nested lambda it's not immediate
      false

    case _ =>
      expr.subexpressions.exists(containsImmediateReturn)
  }

  def apply(parentState : PlannerState, parentPlan : PlanWriter)(fixedArgLocs : List[StorageLocation], restArgLoc : Option[StorageLocation], body : et.Expression, sourceNameHint : Option[String])(implicit planConfig : PlanConfig) : PlanResult = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")

    // Find the variables that are closed by the parent scope
    val refedVars = findRefedVariables(body)

    // Figure out if the immutables need to be captured
    val closedVariables = (refedVars flatMap { storageLoc =>
      parentState.values.get(storageLoc) map {
        case ImmutableValue(parentIntermediate) =>
          parentIntermediate.closureRepresentation match {
            case Some(valueType) =>
              val recordField = new vt.RecordField(storageLoc.sourceName, valueType)

              // We have to capture this
              CapturedImmutable(storageLoc, parentIntermediate, valueType, recordField)

            case None =>
              // No need for capturing - import the intermediate value directly
              ImportedImmutable(storageLoc, parentIntermediate)
          }

      case MutableValue(mutableTemp, needsUndefCheck) =>
        val recordField = new vt.RecordField(storageLoc.sourceName, vt.MutableType)
        CapturedMutable(storageLoc, mutableTemp, recordField, needsUndefCheck)
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
    val procSelfOpt = if (capturedVariables.isEmpty) {
      None
    }
    else {
      Some(ps.GcManagedValue())
    }

    // Make our closure type
    val closureType = if (procSelfOpt.isDefined) {
      val closureSourceName = sourceName + "-closure"
      new vt.ClosureType(closureSourceName, capturedVariables.map(_.recordField))
    }
    else {
      vt.EmptyClosureType
    }

    val allArgs = fixedArgLocs.map({ storageLoc =>
      Argument(storageLoc, ps.GcManagedValue(), vt.IntrinsicCellType(ct.DatumCell))
    }) ++
    restArgLoc.map({ storageLoc =>
      Argument(storageLoc, ps.GcManagedValue(), vt.IntrinsicCellType(ct.ListElementCell))
    })

    // Split our args in to mutable and immutable
    val (mutableArgs, immutableArgs) = allArgs.partition { case Argument(storageLoc, _, _) =>
      planConfig.analysis.mutableVars.contains(storageLoc)
    }

    // Immutable vars can be used immediately
    val importedImmutables = (closedVariables.collect {
      case imported : ImportedImmutable => 
        (imported.storageLoc, ImmutableValue(imported.parentIntermediate))
    }).toMap

    val argImmutables = (immutableArgs.map { case Argument(storageLoc, tempValue, valueType) =>
      (storageLoc, ImmutableValue(TempValueToIntermediate(valueType, tempValue)))
    }).toMap

    // Start a new state for the procedure
    val initialImmutables = argImmutables ++ importedImmutables

    val preMutableState = PlannerState(
      values=initialImmutables,
      worldPtr=worldPtr
    )

    val procPlan = parentPlan.forkPlan() 

    // Initialize all of our mutable parameters
    val postMutableState = initializeMutableArgs(preMutableState)(mutableArgs)(procPlan) 
    
    // Load all of our captured variables
    val postClosureState = procSelfOpt match {
      case None =>
        postMutableState

      case Some(procSelf) => 
        val closureDataTemp = ps.GcUnmanagedValue()
        procPlan.steps += ps.StoreRecordLikeData(closureDataTemp, procSelf, closureType)

        val state = loadClosureData(postMutableState)(closureDataTemp, closureType, capturedVariables)(procPlan)

        // Dispose of our closure cell and data pointer
        procPlan.steps += ps.DisposeValue(procSelf)
        procPlan.steps += ps.DisposeValue(closureDataTemp)

        state
    }

    // Plan the body
    val planResult = PlanExpression(postClosureState)(body)(planConfig, procPlan)

    // Are we returning anything?
    val unitType = vt.IntrinsicCellType(ct.UnitCell)

    val returnTypeOpt = if (containsImmediateReturn(body)) {
      // Return a DatumCell
      // XXX: We can be more clever here and try to find a common return type across all returns
      Some(vt.IntrinsicCellType(ct.DatumCell))
    }
    else if (planResult.value.possibleTypes == Set(unitType)) {
      // Instead of returning a unit cell just return void
      None
    }
    else {
      // Find the preferred representation of our return value
      Some(planResult.value.preferredRepresentation)
    }

    // Return from the function
    procPlan.steps += ps.Return(returnTypeOpt map { returnType =>
      planResult.value.toTempValue(returnType)(procPlan, worldPtr)
    })
    
    // Determine our signature
    val procSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=procSelfOpt.isDefined,
      hasRestArg=restArgLoc.isDefined,
      fixedArgs=fixedArgLocs.map { _ =>
        vt.IntrinsicCellType(ct.DatumCell)
      },
      returnType=returnTypeOpt
    )

    // Name our function arguments
    val namedArguments =
      ("world" -> worldPtr) ::
      procSelfOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      (allArgs.map { case Argument(storageLoc, tempValue, _) =>
        (storageLoc.sourceName -> tempValue)
      })

    val uninferredFunction = PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=procPlan.steps.toList,
      worldPtrOption=Some(worldPtr)
    )

    val plannedFunction = if (planConfig.optimize) {
      // Attempt to infer our argument types
      InferArgumentTypes(uninferredFunction)
    }
    else {
      uninferredFunction
    }

    val nativeSymbol = parentPlan.allocProcedureSymbol(sourceName)
    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction)

    val procCell = procSelfOpt map { _ => 
      // Save the closure values from the parent's scope
      val cellTemp = ps.GcManagedValue()
      val dataTemp = ps.GcUnmanagedValue()

      parentPlan.steps += ps.RecordLikeInit(cellTemp, dataTemp, closureType)

      storeClosureData(dataTemp, closureType, capturedVariables)(parentPlan, parentState.worldPtr)

      cellTemp
    }

    val procValue = new iv.KnownProcedure(plannedFunction.signature, () => nativeSymbol, procCell)

    PlanResult(
      state=parentState,
      value=procValue
    )
  }
}

