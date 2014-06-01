package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, StorageLocation, ReportProcedure, SourceLocated}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{intermediatevalue => iv}

private[planner] object PlanBind {
  private def storageLocRefedByExpr(storageLoc : StorageLocation, expr : et.Expr) : Boolean = expr match {
    case et.VarRef(`storageLoc`) =>
      true

    case nonVarRef => 
      nonVarRef.subexprs.exists(storageLocRefedByExpr(storageLoc, _))
  }

  def apply(initialState : PlannerState)(bindings : List[(StorageLocation, et.Expr)])(implicit planConfig : PlanConfig, plan : PlanWriter) : PlannerState = {
    implicit val worldPtr = initialState.worldPtr

    val bindingLocs = bindings.map(_._1).toSet

    bindings.foldLeft(initialState) { case (prerecursiveState, (storageLoc, initialValue)) =>
      // Check for any recursive values we may have to introduce
      val neededRecursives = (bindingLocs
        .filter(!prerecursiveState.values.contains(_))
        .filter(storageLocRefedByExpr(_, initialValue))
      )

      // Is this a lambda referring to itself recursively and is not a mutable value?
      val isSelfRecursiveLambda = neededRecursives.contains(storageLoc) &&
        !planConfig.analysis.mutableVars.contains(storageLoc) &&
        initialValue.isInstanceOf[et.Lambda]

      val neededNonSelfRecursives = if (isSelfRecursiveLambda) {
        neededRecursives - storageLoc
      }
      else {
        neededRecursives
      }

      val postrecursiveState = neededNonSelfRecursives.foldLeft(prerecursiveState) { case (state, storageLoc) =>
        val recursiveTemp = ps.RecordTemp()

        val recordDataTemp = ps.RecordLikeDataTemp()

        plan.steps += ps.RecordLikeInit(recursiveTemp, recordDataTemp, vt.MutableType)
        // Mark this value as undefined so a runtime error will be raised if it is accessed
        plan.steps += ps.RecordDataFieldSetUndefined(recordDataTemp, vt.MutableType, vt.MutableField)

        state.withValue(storageLoc -> MutableValue(recursiveTemp, true))
      }
      
      val initialValueResult = initialValue match {
        case et.Lambda(fixedArgs, restArg, body) if isSelfRecursiveLambda =>
          PlanLambda(initialState, plan)(
            fixedArgLocs=fixedArgs,
            restArgLoc=restArg,
            body=body,
            sourceNameHint=Some(storageLoc.sourceName),
            recursiveSelfLoc=Some(storageLoc)
          )

        case otherExpr =>
          PlanExpr(postrecursiveState)(otherExpr, Some(storageLoc.sourceName))
      }
        

      // Was this previously a recursive value?
      val prevRecursiveOpt = postrecursiveState.values.get(storageLoc) match {
        case Some(MutableValue(recursiveTemp, true)) =>
          // This was previously a recursive value

          val initialValueTemp = initialValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

          // Update the recursive to point to our new value
          val recordDataTemp = ps.RecordLikeDataTemp()

          plan.steps += ps.StoreRecordLikeData(recordDataTemp, recursiveTemp, vt.MutableType)
          plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, initialValueTemp)

          Some(recursiveTemp)

        case _ =>
          None
      }

      if (planConfig.analysis.mutableVars.contains(storageLoc)) {
        // If we used to be a recursive value we can reuse that record
        val mutableTemp = prevRecursiveOpt.getOrElse {
          val mutableTemp = ps.RecordTemp()
          
          val initialValueTemp = initialValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

          // Create a new mutable
          val recordDataTemp = ps.RecordLikeDataTemp()
          plan.steps += ps.RecordLikeInit(mutableTemp, recordDataTemp, vt.MutableType)

          // Set the value
          plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, initialValueTemp)

          mutableTemp
        }
        
        initialValueResult.state.withValue(storageLoc -> MutableValue(mutableTemp, false))
      }
      else {
        // Send a hint about our name
        val reportNamedValue = (initialValueResult.value, storageLoc) match {
          case (knownProc : iv.KnownProcedure, reportProc : ReportProcedure) =>
            // Annotate with our report name so we can optimize when we try
            // to apply this
            // Note this is agnostic to if the implementation is a native
            // function versus a Scheme procedure
            knownProc.withReportName(reportProc.reportName)

          case (otherValue, _) =>
            otherValue
        }

        // No planning, just remember this intermediate value
        initialValueResult.state.withValue(storageLoc -> ImmutableValue(reportNamedValue))
      }
    }
  }
}
