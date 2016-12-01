package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{et, StorageLocation, StdlibProcedure, ContextLocated}
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
    val bindingLocs = bindings.map(_.storageLoc).toSet

    bindings.foldLeft(initialState) { case (prerecursiveState, binding) =>
      val storageLoc = binding.storageLoc

      // Check for any recursive values we may have to introduce
      val neededRecursives = (bindingLocs
        .filter(!prerecursiveState.values.contains(_))
        .filter(storageLocRefedByExpr(_, binding.initialiser))
      )

      // Is this a lambda referring to itself recursively and is not a mutable value?
      val isSelfRecursiveLambda = neededRecursives.contains(storageLoc) &&
        !plan.config.analysis.mutableVars.contains(storageLoc) &&
        binding.initialiser.isInstanceOf[et.Lambda]

      val neededNonSelfRecursives = if (isSelfRecursiveLambda) {
        neededRecursives - storageLoc
      }
      else {
        neededRecursives
      }

      val postrecursiveState = neededNonSelfRecursives.foldLeft(prerecursiveState) { case (state, storageLoc) =>
        // If we're an immutable value then peek at our initialiser when creating the mutable value. This allows us to
        // get basic signature information from lambdas without explicit type declarations for better error reporting
        // and optimisation
        val valueType = if (plan.config.analysis.mutableVars.contains(storageLoc)) {
          vt.AnySchemeType
        }
        else {
          bindings.find(_.storageLoc == storageLoc).map(_.initialiser.schemeType) getOrElse vt.AnySchemeType
        }

        val compactInnerType = CompactRepresentationForType(storageLoc.schemeType & valueType)
        val mutableType = MutableType(compactInnerType)

        // Mark this value as undefined so a runtime error will be raised if it is accessed
        val recursiveTemp = ps.RecordTemp()
        plan.steps += ps.InitRecord(recursiveTemp, mutableType, Map(), isUndefined=true)

        state.withValue(storageLoc -> MutableValue(mutableType, recursiveTemp, true))
      }

      val initialValueResult = binding match {
        case et.Binding(storageLoc, lambdaExpr : et.Lambda) if isSelfRecursiveLambda =>
          plan.withContextLocation(binding.initialiser) {
            val procValue = PlanLambda(postrecursiveState, plan)(
              lambdaExpr=lambdaExpr,
              sourceNameHint=Some(storageLoc.sourceName),
              recursiveSelfLocOpt=Some(storageLoc)
            )

            PlanResult(
              state=postrecursiveState,
              value=procValue
            )
          }

        case et.Binding(storageLoc, otherExpr) =>
          PlanExpr(postrecursiveState)(otherExpr, Some(storageLoc.sourceName))
      }


      // Convert the result values to a single value
      val uncastIntermediate = initialValueResult.value

      // And cast to the correct type
      val initialIntermediate = uncastIntermediate.castToSchemeType(storageLoc.schemeType)

      val storageLocToValue = Map(storageLoc -> initialIntermediate)

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
            val fieldValues = Map[vt.RecordField, ps.TempValue](mutableType.recordField -> initialValueTemp)
            plan.steps += ps.InitRecord(mutableTemp, mutableType, fieldValues, isUndefined=false)

            MutableValue(mutableType, mutableTemp, false)
          }

          state.withValue(storageLoc -> mutableValue)
        }
        else {
          // Send a hint about our name
          val stdlibNamedValue = (initialIntermediate, storageLoc) match {
            case (userProc : iv.KnownUserProc, stdlibProc : StdlibProcedure) =>
              // Annotate with our stdlib name so we can optimise when we try to apply this
              // Note this is agnostic to if the implementation is a native function versus a Scheme procedure
              userProc.withStdlibName(stdlibProc.stdlibName)

            case (otherValue, _) =>
              otherValue
          }

          // No planning, just remember this intermediate value
          state.withValue(storageLoc -> ImmutableValue(stdlibNamedValue))
        }
      }
    }
  }
}
