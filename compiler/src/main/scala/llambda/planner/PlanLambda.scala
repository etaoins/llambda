package llambda.planner

import llambda.et
import llambda.{valuetype => vt}
import llambda.{celltype => ct}
import llambda.planner.{step => ps}
import llambda.{StorageLocation, ProcedureSignature}

private[planner] object PlanLambda {
  case class Argument(
    storageLoc : StorageLocation,
    tempValue : ps.TempValue,
    valueType : vt.ValueType
  )

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

  def apply(parentState : PlannerState)(fixedArgLocs : List[StorageLocation], restArgLoc : Option[StorageLocation], body : et.Expression)(implicit planConfig : PlanConfig) : PlannedFunction = {
    // Setup our arguments
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

    // Find the variables that are closed by the parent scope
    val refedVars = findRefedVariables(body)

    val closedByImmutables = refedVars intersect parentState.immutables.keySet

    // Import the immutabes that don't need to be captured
    val importedImmutables = (closedByImmutables flatMap { storageLoc =>
      val parentIntermediate = parentState.immutables(storageLoc)

      parentIntermediate.closureRepresentation match {
        case Some(valueType) => 
          Nil

        case None =>
          (storageLoc -> parentIntermediate) :: Nil
      }
    }).toMap

    // Start a new state for the procedure
    val initialImmutables = argImmutables ++ importedImmutables
    val preMutableState = PlannerState(immutables=initialImmutables) 

    implicit val plan = PlanWriter()

    // Initialize aoll of our mutable parameters
    val postMutableState = initializeMutableArgs(preMutableState)(mutableArgs) 
        
    // Plan the body
    val planResult = PlanExpression(postMutableState)(body)(planConfig, plan)

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
    plan.steps += ps.Return(returnTypeOpt map { returnType =>
      planResult.value.toRequiredTempValue(returnType)
    })
    
    // Determine our signature
    val procSignature = new ProcedureSignature {
      val hasClosureArg : Boolean = false
      val hasRestArg : Boolean = restArgLoc.isDefined

      val fixedArgs : List[vt.ValueType] = fixedArgLocs.map { _ =>
        vt.IntrinsicCellType(ct.DatumCell)
      }

      val returnType = returnTypeOpt
    }

    // Name our function arguments
    val namedArguments = allArgs.map { case Argument(storageLoc, tempValue, _) =>
      (storageLoc.sourceName -> tempValue)
    }

    PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=plan.steps.toList
    )
  }
}

