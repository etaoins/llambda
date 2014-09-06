package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, StorageLocation, ReportProcedure, ContextLocated}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.codegen.CompactRepresentationForType

private[planner] object PlanBind {
  private def storageLocRefedByExpr(storageLoc : StorageLocation, expr : et.Expr) : Boolean = expr match {
    case et.VarRef(`storageLoc`) =>
      true

    case nonVarRef => 
      nonVarRef.subexprs.exists(storageLocRefedByExpr(storageLoc, _))
  }

  def apply(initialState : PlannerState)(bindings : List[(StorageLocation, et.Expr)])(implicit plan : PlanWriter) : PlannerState = {
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
        !plan.config.analysis.mutableVars.contains(storageLoc) &&
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

        // Mark this value as undefined so a runtime error will be raised if it is accessed
        val compactInnerType = CompactRepresentationForType(storageLoc.schemeType)
        val mutableType = MutableType(compactInnerType)

        plan.steps += ps.InitRecordLike(recursiveTemp, recordDataTemp, mutableType, isUndefined=true)

        state.withValue(storageLoc -> MutableValue(mutableType, recursiveTemp, true))
      }
      
      val initialValueResult = initialValue match {
        case lambdaExpr : et.Lambda if isSelfRecursiveLambda =>
          plan.withContextLocation(initialValue) {
            PlanLambda(initialState, plan)(
              lambdaExpr=lambdaExpr,
              sourceNameHint=Some(storageLoc.sourceName),
              recursiveSelfLoc=Some(storageLoc)
            )
          }

        case otherExpr =>
          PlanExpr(postrecursiveState)(otherExpr, Some(storageLoc.sourceName))
      }

      // Convert the result values to a single value
      val uncastIntermediate = initialValueResult.values.toIntermediateValue()

      // And cast to the correct type
      val initialIntermediate = uncastIntermediate.castToSchemeType(storageLoc.schemeType) 

      // Was this previously a recursive value?
      val prevRecursiveOpt = postrecursiveState.values.get(storageLoc) match {
        case Some(mutableValue @ MutableValue(mutableType, recursiveTemp, true)) =>
          // This was previously a recursive value

          val initialValueTemp = initialIntermediate.toTempValue(mutableType.innerType)

          // Update the recursive to point to our new value
          val recordDataTemp = ps.RecordLikeDataTemp()

          plan.steps += ps.LoadRecordLikeData(recordDataTemp, recursiveTemp, mutableType)
          plan.steps += ps.SetRecordDataField(recordDataTemp, mutableType, mutableType.recordField, initialValueTemp)

          // Mark us as defined
          plan.steps += ps.SetRecordLikeDefined(recursiveTemp, mutableType)

          // We no longer need an undef chaeck 
          Some(mutableValue.copy(needsUndefCheck=false))

        case _ =>
          None
      }

      if (plan.config.analysis.mutableVars.contains(storageLoc)) {
        // If we used to be a recursive value we can reuse that record
        val mutableValue = prevRecursiveOpt.getOrElse {
          val mutableTemp = ps.RecordTemp()
          
          val compactInnerType = CompactRepresentationForType(storageLoc.schemeType)
          val mutableType = MutableType(compactInnerType)
          
          val initialValueTemp = initialIntermediate.toTempValue(compactInnerType)

          // Create a new mutable
          val recordDataTemp = ps.RecordLikeDataTemp()
          plan.steps += ps.InitRecordLike(mutableTemp, recordDataTemp, mutableType, isUndefined=false)

          // Set the value
          plan.steps += ps.SetRecordDataField(recordDataTemp, mutableType, mutableType.recordField, initialValueTemp)

          MutableValue(mutableType, mutableTemp, false)
        }
        
        initialValueResult.state.withValue(storageLoc -> mutableValue)
      }
      else {
        // Send a hint about our name
        val reportNamedValue = (initialIntermediate, storageLoc) match {
          case (userProc : iv.KnownUserProc, reportProc : ReportProcedure) =>
            // Annotate with our report name so we can optimize when we try to apply this
            // Note this is agnostic to if the implementation is a native function versus a Scheme procedure
            userProc.withReportName(reportProc.reportName)

          case (otherValue, _) =>
            otherValue
        }

        // No planning, just remember this intermediate value
        initialValueResult.state.withValue(storageLoc -> ImmutableValue(reportNamedValue))
      }
    }
  }
}
