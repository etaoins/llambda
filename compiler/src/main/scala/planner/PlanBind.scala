package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, StorageLocation, ReportProcedure, ContextLocated}
import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
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

  def apply(initialState : PlannerState)(bindings : List[et.Binding])(implicit plan : PlanWriter) : PlannerState = {
    val bindingLocs = bindings.flatMap(_.storageLocs).toSet

    bindings.foldLeft(initialState) { case (prerecursiveState, binding) =>
      // Check for any recursive values we may have to introduce
      val neededRecursives = (bindingLocs
        .filter(!prerecursiveState.values.contains(_))
        .filter(storageLocRefedByExpr(_, binding.initialiser))
      )

      // Is this a lambda referring to itself recursively and is not a mutable value?
      val (isSelfRecursiveLambda, neededNonSelfRecursives) = binding match {
        case et.SingleBinding(storageLoc, _) =>
          val isSelfRecursiveLambda = neededRecursives.contains(storageLoc) &&
            !plan.config.analysis.mutableVars.contains(storageLoc) &&
            binding.initialiser.isInstanceOf[et.Lambda]

          val neededNonSelfRecursives = if (isSelfRecursiveLambda) {
            neededRecursives - storageLoc
          }
          else {
            neededRecursives
          }

          (isSelfRecursiveLambda, neededNonSelfRecursives)

        case _ : et.MultipleValueBinding =>
          // Multiple value bindings can't be recursive
          (false, neededRecursives)
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
      
      val initialValueResult = binding match {
        case et.SingleBinding(storageLoc, lambdaExpr : et.Lambda) if isSelfRecursiveLambda =>
          plan.withContextLocation(binding.initialiser) {
            val procValue = PlanLambda(postrecursiveState, plan)(
              lambdaExpr=lambdaExpr,
              sourceNameHint=Some(storageLoc.sourceName),
              recursiveSelfLocOpt=Some(storageLoc)
            )

            PlanResult(
              state=postrecursiveState,
              values=SingleValue(procValue)
            )
          }

        case et.SingleBinding(storageLoc, otherExpr) =>
          PlanExpr(postrecursiveState)(otherExpr, Some(storageLoc.sourceName))

        case et.MultipleValueBinding(_, _, initialiser) =>
          // Multiple value bindings don't support source names as one expression maps to multiple storage locations
          PlanExpr(postrecursiveState)(initialiser, None)
      }

      val storageLocToValue = binding match {
        case et.SingleBinding(storageLoc, _) =>
          // Convert the result values to a single value
          val uncastIntermediate = initialValueResult.values.toSingleValue()

          // And cast to the correct type
          val initialIntermediate = uncastIntermediate.castToSchemeType(storageLoc.schemeType)

          Map(storageLoc -> initialIntermediate)

        case et.MultipleValueBinding(fixedLocs, restLocOpt, _) =>
          val fixedValueCount = fixedLocs.length
          val memberTypes = fixedLocs.map(_.schemeType)

          val insufficientValuesMessage = if (restLocOpt.isDefined) {
            RuntimeErrorMessage(
              category=ErrorCategory.Arity,
              name=s"insufficientValuesRequiresAtLeast${fixedValueCount}",
              text=s"Insufficient values for multiple value binding; requires at least ${fixedValueCount} values."
            )
          }
          else {
            RuntimeErrorMessage(
              category=ErrorCategory.Arity,
              name=s"insufficientValueRequiresExactly${fixedValueCount}",
              text=s"Insufficient values for multiple value binding; requires exactly ${fixedValueCount} values."
            )
          }

          val destructureResult = DestructureList(
            initialValueResult.values.toMultipleValueList,
            memberTypes,
            insufficientValuesMessage
          )

          if (!restLocOpt.isDefined) {
            // Make sure we don't have extra values
            val tooManyValuesMessage = RuntimeErrorMessage(
              category=ErrorCategory.Arity,
              s"tooManyValuesRequiresExactly${fixedValueCount}",
              s"Too many values in multiple value binding; requires exactly ${fixedValueCount} values."
            )

            destructureResult.listTailValue.toTempValue(vt.EmptyListType, Some(tooManyValuesMessage))
          }

          val storageLocToFixedValue = (fixedLocs zip destructureResult.memberTemps) map {
            case (storageLoc, tempValue) =>
              storageLoc -> TempValueToIntermediate(storageLoc.schemeType, tempValue)(plan.config)
          }

          val storageLocToRestValue = restLocOpt map { restLoc =>
            restLoc -> destructureResult.listTailValue.castToSchemeType(restLoc.schemeType)
          }

          storageLocToFixedValue ++ storageLocToRestValue
      }

      // Bind each intermediate value to its storage location
      storageLocToValue.foldLeft(initialValueResult.state) { case (state, (storageLoc, initialIntermediate)) =>
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

          state.withValue(storageLoc -> mutableValue)
        }
        else {
          // Send a hint about our name
          val reportNamedValue = (initialIntermediate, storageLoc) match {
            case (userProc : iv.KnownUserProc, reportProc : ReportProcedure) =>
              // Annotate with our report name so we can optimise when we try to apply this
              // Note this is agnostic to if the implementation is a native function versus a Scheme procedure
              userProc.withReportName(reportProc.reportName)

            case (otherValue, _) =>
              otherValue
          }

          // No planning, just remember this intermediate value
          state.withValue(storageLoc -> ImmutableValue(reportNamedValue))
        }
      }
    }
  }
}
