package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.{et, StorageLocation, ReportProcedure, ContextLocated, ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.InternalCompilerErrorException

private[planner] object PlanExpr {
  def apply(initialState : PlannerState)(expr : et.Expr, sourceNameHint : Option[String] = None)(implicit plan : PlanWriter) : PlanResult = plan.withContextLocation(expr) {
    expr match {
      case et.Begin(exprs) =>
        val initialResult = PlanResult(
          state=initialState,
          value=iv.UnitValue
        )

        exprs.foldLeft(initialResult) { case (planResult, expr) =>
          if (planResult.value == iv.UnreachableValue) {
            // This code is unreachable - check if the code is valid but keep track of the fact we're not reachable
            apply(planResult.state)(expr).copy(value=iv.UnreachableValue)
          }
          else {
            apply(planResult.state)(expr)
          }
        }

      case et.Apply(procExpr, argExprs) =>
        PlanApplication(initialState)(procExpr, argExprs)

      case et.TopLevelDefine(binding) =>
        PlanResult(
          state=PlanBind(initialState)(List(binding)),
          value=iv.UnitValue
        )

      case et.InternalDefine(bindings, bodyExpr) =>
        val bodyState = PlanBind(initialState)(bindings)

        // Return to our initial state
        // InternalDefines can re-bind existing variables and they must be restored after its body is planned
        PlanResult(
          state=initialState,
          value=apply(bodyState)(bodyExpr).value
        )

      case et.VarRef(storageLoc : StorageLocation) => 
        initialState.values(storageLoc) match {
          case ImmutableValue(value) =>
            // Return the value directly 
            PlanResult(
              state=initialState,
              value=value
            )

          case MutableValue(mutableType, mutableTemp, needsUndefCheck) =>
            if (needsUndefCheck) {
              val errorMessage = RuntimeErrorMessage(
                category=ErrorCategory.UndefinedVariable,
                name="accessUndefined",
                text="Recursively defined value referenced before its initialization"
              )

              plan.steps += ps.AssertRecordLikeDefined(mutableTemp, mutableType, errorMessage)
            }

            // Load our data pointer
            val recordDataTemp = ps.RecordLikeDataTemp()
            plan.steps += ps.LoadRecordLikeData(recordDataTemp, mutableTemp, mutableType)

            // Load the data
            val resultTemp = ps.Temp(mutableType.innerType)
            plan.steps += ps.LoadRecordDataField(resultTemp, recordDataTemp, mutableType, mutableType.recordField)

            val resultValue = TempValueToIntermediate(mutableType.innerType, resultTemp)(plan.config)

            PlanResult(
              state=initialState,
              value=resultValue
            )
        }
      
      case et.MutateVar(storageLoc, valueExpr) =>
        val mutableValue = initialState.values(storageLoc) match {
          case mutable : MutableValue  =>
            mutable

          case _ =>
            throw new InternalCompilerErrorException(s"Attempted to mutate non-mutable: ${storageLoc}")
        }
        
        val mutableTemp = mutableValue.mutableTemp
        val mutableType = mutableValue.mutableType

        // Evaluate at convert to the correct type for the mutable
        val newValueResult = apply(initialState)(valueExpr)
        val newValueIntermediate = newValueResult.value
        val newValueTemp = newValueIntermediate.toTempValue(mutableType.innerType)

        // Load our data pointer
        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(recordDataTemp, mutableTemp, mutableType)

        // Store the data
        plan.steps += ps.SetRecordDataField(recordDataTemp, mutableType, mutableType.recordField, newValueTemp)

        PlanResult(
          state=newValueResult.state,
          value=iv.UnitValue
        )

      case et.Literal(value) =>
        PlanResult(
          state=initialState,
          value=DatumToConstantValue(value)
        )

      case et.Cond(testExpr, trueExpr, falseExpr) =>
        PlanCond(initialState)(testExpr, trueExpr, falseExpr)

      case nativeFunc : et.NativeFunction =>
        plan.requiredNativeLibraries += nativeFunc.library
        val newProcValue = new iv.KnownUserProc(nativeFunc.polySignature, nativeFunc.nativeSymbol, None)

        PlanResult(
          state=initialState,
          value=newProcValue
        )

      case recordConstructor @ et.RecordConstructor(recordType, initializedFields) =>
        val newProcValue = new iv.KnownRecordConstructorProc(
          recordType=recordType,
          initializedFields=initializedFields
        )

        PlanResult(
          state=initialState,
          value=newProcValue
        )

      case recordAccessor @ et.RecordAccessor(recordType, field) =>
        val newProcValue =new iv.KnownRecordAccessorProc(
          recordType=recordType,
          field=field
        )

        PlanResult(
          state=initialState,
          value=newProcValue
        )

      case recordMutator @ et.RecordMutator(recordType, field) =>
        val newProcValue = new iv.KnownRecordMutatorProc(
          recordType=recordType,
          field=field
        )

        PlanResult(
          state=initialState,
          value=newProcValue
        )

      case typePredicate @ et.TypePredicate(schemeType) =>
        val newProcValue = new iv.KnownTypePredicateProc(schemeType)

        PlanResult(
          state=initialState,
          value=newProcValue
        )

      case et.Cast(valueExpr, targetType, staticCheck) =>
        val valueResult = apply(initialState)(valueExpr)
        val valueIntermediate = valueResult.value
        val castValue = valueIntermediate.castToSchemeType(targetType, None, staticCheck)

        PlanResult(
          state=valueResult.state,
          value=castValue
        )

      case lambdaExpr : et.Lambda =>
        val procValue = PlanLambda(initialState, plan)(
          lambdaExpr=lambdaExpr,
          sourceNameHint=sourceNameHint,
          recursiveSelfLocOpt=None
        )

        PlanResult(
          state=initialState,
          value=procValue
        )

      case et.CaseLambda(clauseExprs) =>
        val procValue = PlanCaseLambda(initialState, plan)(
          clauseExprs=clauseExprs,
          sourceNameHint=sourceNameHint
        )

        PlanResult(
          state=initialState,
          value=procValue
        )

      case et.Parameterize(parameterValues, innerExpr) =>
        val parameterValueTemps = new mutable.ListBuffer[ps.ParameterizedValue]

        val postValueState = parameterValues.foldLeft(initialState) { case (state, (parameterExpr, valueExpr)) =>
          val parameterResult = apply(state)(parameterExpr)
          val valueResult = apply(parameterResult.state)(valueExpr)

          val parameterIntermediate = parameterResult.value
          val parameterTemp = parameterIntermediate.toTempValue(vt.SchemeTypeAtom(ct.ProcedureCell), convertProcType=false)

          val mayHaveConverterProc = parameterIntermediate match {
            case knownParamProc : iv.KnownParameterProc =>
              knownParamProc.hasConverter

            case _ =>
              true
          }

          val valueIntermediate = valueResult.value
          val valueTemp = valueIntermediate.toTempValue(vt.AnySchemeType)

          parameterValueTemps += ps.ParameterizedValue(parameterTemp, valueTemp, mayHaveConverterProc)

          valueResult.state
        }

        plan.steps += ps.PushDynamicState(parameterValueTemps.toList)
        val postInnerResult = PlanExpr(postValueState)(innerExpr)
        plan.steps += ps.PopDynamicState()

        postInnerResult
    }
  }
}
