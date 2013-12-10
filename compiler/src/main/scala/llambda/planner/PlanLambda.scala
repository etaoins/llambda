package llambda.planner

import llambda.et
import llambda.{valuetype => vt}
import llambda.{celltype => ct}
import llambda.planner.{step => ps}
import llambda.{StorageLocation, ProcedureSignature}

private[planner] object PlanLambda {
  def findRefedVariables(expr : et.Expression) : Set[StorageLocation] = expr match {
    case et.VarRef(variable) =>
      Set(variable)

    case otherExpr =>
      otherExpr.subexpressions.flatMap(findRefedVariables).toSet
  }

  def apply(parentState : PlannerState)(fixedArgLocs : List[StorageLocation], restArgLoc : Option[StorageLocation], body : et.Expression)(implicit planConfig : PlanConfig) : PlannedFunction = {
    // Setup our arguments
    val fixedArgTemps = fixedArgLocs map { storageLoc =>
      (storageLoc, new ps.TempValue)
    }

    val restArgTemp = restArgLoc map { storageLoc =>
      (storageLoc, new ps.TempValue)
    }

    val argImmutables = (fixedArgTemps.map { case (storageLoc, tempValue) =>
      val argType = vt.IntrinsicCellType(ct.DatumCell)
      (storageLoc, TempValueToIntermediate(argType, tempValue))
    } ++
    restArgTemp.map { case (storageLoc, tempValue) =>
      val argType = vt.IntrinsicCellType(ct.ListElementCell)
      (storageLoc, TempValueToIntermediate(argType, tempValue))
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
    val state = PlannerState(immutables=initialImmutables) 

    implicit val plan = PlanWriter()
        
    // Plan the body
    val planResult = PlanExpression(state)(body)(planConfig, plan)

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
    val namedArguments = (fixedArgTemps ++ restArgTemp.toList).map { case (storageLoc, tempValue) =>
      (storageLoc.sourceName -> tempValue)
    }

    PlannedFunction(
      signature=procSignature,
      namedArguments=namedArguments,
      steps=plan.steps.toList
    )
  }
}

