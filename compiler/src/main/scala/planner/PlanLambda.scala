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

  private sealed abstract class CapturedVariable {
    val storageLoc : StorageLocation
    val valueType : vt.ValueType
    val recordField : vt.RecordField
  }

  private case class CapturedImmutatable(
    storageLoc : StorageLocation,
    parentIntermediate : iv.IntermediateValue,
    valueType : vt.ValueType,
    recordField : vt.RecordField
  ) extends CapturedVariable
  
  private case class CapturedMutable(
    storageLoc : StorageLocation,
    tempValue : ps.TempValue,
    recordField : vt.RecordField
  ) extends CapturedVariable {
    val valueType = vt.MutableType
  }

  private def findRefedVariables(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(variable) =>
      Set(variable)

    case otherExpr =>
      otherExpr.subexpressions.flatMap(findRefedVariables).toSet
  }

  private def initializeMutableArgs(initialState : PlannerState)(mutableArgs : List[Argument])(implicit plan : PlanWriter) : PlannerState = mutableArgs.length match {
    case 0 =>
      // Nothing to do
      initialState

    case argCount =>
      // Allocate all of our space
      val allocTemp = new ps.TempAllocation
      plan.steps += ps.AllocateCells(allocTemp, argCount)

      mutableArgs.zipWithIndex.foldLeft(initialState) { case (state, (Argument(storageLoc, tempValue, _), index)) =>
        // Init the mutable
        val mutableTemp = new ps.TempValue
        val recordDataTemp = new ps.TempValue

        plan.steps += ps.RecordLikeInit(mutableTemp, recordDataTemp, allocTemp, index, vt.MutableType)

        // Set the value
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, tempValue)
        
        state.withMutable(storageLoc -> mutableTemp)
      }
  }

  private def loadClosureData(initialState : PlannerState)(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter) : PlannerState = 
    capturedVariables.foldLeft(initialState) { case (state, capturedVar) =>
      // Load the variable
      val varTemp = new ps.TempValue
      plan.steps += ps.RecordDataFieldRef(varTemp, closureDataTemp, closureType, capturedVar.recordField) 

      // Add it to our state
      capturedVar match {
        case _ : CapturedImmutatable =>
          val varValue = TempValueToIntermediate(capturedVar.valueType, varTemp) 
          state.withImmutable(capturedVar.storageLoc -> varValue)

        case _ : CapturedMutable => 
          state.withMutable(capturedVar.storageLoc -> varTemp)
      }
    }
  
  private def storeClosureData(closureDataTemp : ps.TempValue, closureType : vt.ClosureType, capturedVariables : List[CapturedVariable])(implicit plan : PlanWriter) : Unit = {
    for(capturedVar <- capturedVariables) {
      val varTemp = capturedVar match {
        case immutable : CapturedImmutatable =>
          // Cast the value to its preferred type
          immutable.parentIntermediate.toRequiredTempValue(capturedVar.valueType)

        case mutable : CapturedMutable =>
          // Store the pointer to the mutable directly
          mutable.tempValue
      }
        
      // Store to the field
      plan.steps += ps.RecordDataFieldSet(closureDataTemp, closureType, capturedVar.recordField, varTemp)
    }
  }

  def apply(parentState : PlannerState, parentPlan : PlanWriter)(fixedArgLocs : List[StorageLocation], restArgLoc : Option[StorageLocation], body : et.Expression, sourceNameHint : Option[String])(implicit planConfig : PlanConfig) : PlanResult = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")

    // Find the variables that are closed by the parent scope
    val refedVars = findRefedVariables(body)

    val closedByImmutables = refedVars intersect parentState.immutables.keySet

    // Figure out if the immutables need to be captured
    val processedImmutables = (closedByImmutables map { storageLoc =>
      val parentIntermediate = parentState.immutables(storageLoc)

      parentIntermediate.closureRepresentation match {
        case Some(valueType) =>
          val recordField = new vt.RecordField(storageLoc.sourceName, valueType)

          // We have to capture this
          Right(CapturedImmutatable(storageLoc, parentIntermediate, valueType, recordField))

        case None =>
          // No need for capturing
          Left((storageLoc -> parentIntermediate))
      }
    })
    
    // Import the immutabes that don't need to be captured
    val importedImmutables = processedImmutables.collect({ case Left(importedTuple) =>
        importedTuple
    }).toMap

    // Make a list of all the captured variables
    val capturedImmutables = processedImmutables.collect({ case Right(capturedVariable) =>
      capturedVariable
    }).toList : List[CapturedVariable] 

    val capturedMutables = ((refedVars intersect parentState.mutables.keySet) map { storageLoc =>
      val recordField = new vt.RecordField(storageLoc.sourceName, vt.MutableType)

      CapturedMutable(storageLoc, parentState.mutables(storageLoc), recordField)
    }).toList : List[CapturedVariable]

    val capturedVariables = capturedImmutables ++ capturedMutables

    // Determine if we have a closure
    val procSelfOpt = if (capturedVariables.isEmpty) {
      None
    }
    else {
      Some(new ps.TempValue)
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
      Argument(storageLoc, new ps.TempValue, vt.IntrinsicCellType(ct.DatumCell))
    }) ++
    restArgLoc.map({ storageLoc =>
      Argument(storageLoc, new ps.TempValue, vt.IntrinsicCellType(ct.ListElementCell))
    })

    // Split our args in to mutable and immutable
    val (mutableArgs, immutableArgs) = allArgs.partition { case Argument(storageLoc, _, _) =>
      planConfig.analysis.mutableVars.contains(storageLoc)
    }

    // Immutable vars can be used immediately
    val argImmutables = (immutableArgs.map { case Argument(storageLoc, tempValue, valueType) =>
      (storageLoc, TempValueToIntermediate(valueType, tempValue))
    }).toMap

    // Start a new state for the procedure
    val initialImmutables = argImmutables ++ importedImmutables
    val preMutableState = PlannerState(immutables=initialImmutables) 

    val procPlan = parentPlan.forkPlan() 

    // Initialize aoll of our mutable parameters
    val postMutableState = initializeMutableArgs(preMutableState)(mutableArgs)(procPlan) 
    
    // Load all of our captured variables
    val postClosureState = procSelfOpt match {
      case None =>
        postMutableState

      case Some(procSelf) => 
        val closureDataTemp = new ps.TempValue
        procPlan.steps += ps.StoreRecordLikeData(closureDataTemp, procSelf, closureType)

        loadClosureData(postMutableState)(closureDataTemp, closureType, capturedVariables)(procPlan)
    }

    // Plan the body
    val planResult = PlanExpression(postClosureState)(body)(planConfig, procPlan)

    // Are we returning anything?
    val unspecificType = vt.IntrinsicCellType(ct.UnspecificCell)
    val returnTypeOpt = if (planResult.value.possibleTypes == Set(unspecificType)) {
      None
    }
    else {
      // Find the preferred representation of our return value
      Some(planResult.value.preferredRepresentation)
    }

    // Return from the function
    procPlan.steps += ps.Return(returnTypeOpt map { returnType =>
      planResult.value.toRequiredTempValue(returnType)(procPlan)
    })
    
    // Determine our signature
    val procSignature = new ProcedureSignature {
      val hasSelfArg = procSelfOpt.isDefined
      val hasRestArg = restArgLoc.isDefined

      val fixedArgs =fixedArgLocs.map { _ =>
        vt.IntrinsicCellType(ct.DatumCell)
      }

      val returnType = returnTypeOpt
    }

    // Name our function arguments
    val namedArguments = procSelfOpt.toList.map({ procSelf =>
      ("self" -> procSelf)
    }) ++
    (allArgs.map { case Argument(storageLoc, tempValue, _) =>
      (storageLoc.sourceName -> tempValue)
    })

    val plannedFunction = PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=procPlan.steps.toList
    )

    val nativeSymbol = parentPlan.allocProcedureSymbol(sourceName)
    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction)

    val procCell = procSelfOpt map { _ => 
      // Save the closure values from the parent's scope
      val tempAllocation = new ps.TempAllocation
      parentPlan.steps += ps.AllocateCells(tempAllocation, 1)

      val cellTemp = new ps.TempValue
      val dataTemp = new ps.TempValue

      parentPlan.steps += ps.RecordLikeInit(cellTemp, dataTemp, tempAllocation, 0, closureType)

      storeClosureData(dataTemp, closureType, capturedVariables)(parentPlan)

      cellTemp
    }

    val procValue = new iv.KnownProcedure(plannedFunction.signature, nativeSymbol, procCell)

    PlanResult(
      state=parentState,
      value=procValue
    )
  }
}

