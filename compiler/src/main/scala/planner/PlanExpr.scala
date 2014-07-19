package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.{et, StorageLocation, ReportProcedure, ContextLocated, RuntimeErrorMessage}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.InternalCompilerErrorException

private[planner] object PlanExpr {
  private val callCcNames = Set("call-with-current-continuation", "call/cc")
  
  def apply(initialState : PlannerState)(expr : et.Expr, sourceNameHint : Option[String] = None)(implicit plan : PlanWriter) : PlanResult = plan.withContextLocation(expr) {
    implicit val worldPtr = initialState.worldPtr

    expr match {
      case et.Begin(exprs) =>
        var finalValue : iv.IntermediateValue = iv.UnitValue

        val finalState = exprs.foldLeft(initialState) { case (state, expr) =>
          val result = apply(state)(expr)
          finalValue = result.value

          result.state
        }

        PlanResult(state=finalState, value=finalValue)

      case et.Apply(procRef @ et.VarRef(callCcProc : ReportProcedure), operands) if callCcNames.contains(callCcProc.reportName) && plan.config.optimize =>
        // This is a (call/cc)
        ReduceCallCc(expr, operands)(plan.config) match {
          case SimplifiedCallCc(newOperands) =>
            PlanApplication(initialState)(expr, procRef, newOperands)
          
          case StrippedCallCc(newExpr) =>
            PlanExpr(initialState)(newExpr)
        }

      case et.Apply(procExpr, operandExprs) =>
        PlanApplication(initialState)(expr, procExpr, operandExprs)

      case et.TopLevelDefine(bindings) =>
        PlanResult(
          state=PlanBind(initialState)(bindings),
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
              val errorMessage = RuntimeErrorMessage("accessUndefined", "Recursively defined value referenced before its initialization") 
              plan.steps += ps.AssertRecordLikeDefined(worldPtr, mutableTemp, mutableType, errorMessage)
            }
            
            // Load our data pointer
            val recordDataTemp = ps.RecordLikeDataTemp()
            plan.steps += ps.LoadRecordLikeData(recordDataTemp, mutableTemp, mutableType)
            
            // Load the data
            val resultTemp = ps.Temp(mutableType.innerType)
            plan.steps += ps.LoadRecordDataField(resultTemp, recordDataTemp, mutableType, mutableType.recordField)

            PlanResult(
              state=initialState,
              value=TempValueToIntermediate(mutableType.innerType, resultTemp)
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
        val newValueTemp = newValueResult.value.toTempValue(mutableType.innerType)

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
        val testResult = apply(initialState)(testExpr)

        testResult.value.schemeType.satisfiesType(vt.ConstantBooleanType(false)) match {
          case Some(true) =>
            // The test result must be false
            PlanExpr(testResult.state)(falseExpr)

          case Some(false) =>
            // The test result can't be false
            PlanExpr(testResult.state)(trueExpr)

          case None =>
            val truthyPred = testResult.value.toTempValue(vt.Predicate)

            val trueWriter = plan.forkPlan()
            val trueValue = apply(testResult.state)(trueExpr)(trueWriter).value

            val falseWriter = plan.forkPlan() 
            val falseValue = apply(testResult.state)(falseExpr)(falseWriter).value
        
            val planPhiResult = trueValue.planPhiWith(falseValue)(trueWriter, falseWriter)

            plan.steps += ps.CondBranch(
              planPhiResult.resultTemp,
              truthyPred,
              trueWriter.steps.toList, planPhiResult.ourTempValue,
              falseWriter.steps.toList, planPhiResult.theirTempValue
            )

            PlanResult(
              state=testResult.state,
              value=planPhiResult.resultIntermediate
            )
        }
      
      case nativeFunc : et.NativeFunction =>
        PlanResult(
          state=initialState,
          value=new iv.KnownUserProc(nativeFunc.signature, nativeFunc.nativeSymbol, None)
        )

      case recordConstructor @ et.RecordConstructor(recordType, initializedFields) =>
        PlanResult(
          state=initialState,
          value=new iv.KnownRecordConstructorProc(
            recordType=recordType,
            initializedFields=initializedFields
          )
        )
      
      case recordAccessor @ et.RecordAccessor(recordType, field) =>
        PlanResult(
          state=initialState,
          value=new iv.KnownRecordAccessorProc(
            recordType=recordType,
            field=field
          )
        )
      
      case recordMutator @ et.RecordMutator(recordType, field) =>
        PlanResult(
          state=initialState,
          value=new iv.KnownRecordMutatorProc(
            recordType=recordType,
            field=field
          )
        )
      
      case typePredicate @ et.TypePredicate(schemeType) =>
        val knownProcedure = plan.plannedTypePredicates.getOrElseUpdate(schemeType, {
          new iv.KnownTypePredicateProc(
            testingType=schemeType
          )
        })

        PlanResult(
          state=initialState,
          value=knownProcedure
        )

      case et.Cast(valueExpr, targetType) =>
        val valueResult = apply(initialState)(valueExpr)
        val castValue = valueResult.value.castToSchemeType(targetType)
          
        PlanResult(state=valueResult.state, value=castValue)

      case lambdaExpr : et.Lambda =>
        PlanLambda(initialState, plan)(
          lambdaExpr=lambdaExpr,
          sourceNameHint=sourceNameHint,
          recursiveSelfLoc=None
        )

      case et.Parameterize(parameterValues, innerExpr) => 
        val parameterValueTemps = new mutable.ListBuffer[(ps.TempValue, ps.TempValue)]

        val postValueState = parameterValues.foldLeft(initialState) { case (state, (parameterExpr, valueExpr)) =>
          val parameterResult = apply(state)(parameterExpr)
          val valueResult = apply(parameterResult.state)(valueExpr)

          val parameterTemp = parameterResult.value.toTempValue(vt.ProcedureType)
          val valueTemp = valueResult.value.toTempValue(vt.AnySchemeType)

          parameterValueTemps += ((parameterTemp, valueTemp))

          valueResult.state
        }

        plan.steps += ps.PushDynamicState(worldPtr, parameterValueTemps.toList)
        val postInnerResult = PlanExpr(postValueState)(innerExpr)
        plan.steps += ps.PopDynamicState(worldPtr)

        postInnerResult

      case et.Return(returnedExpr) =>
        val returnValueResult = apply(initialState)(returnedExpr)
        // If there's a return the return type is always DatumCell
        val returnValueTemp = returnValueResult.value.toTempValue(vt.AnySchemeType)

        plan.steps += ps.Return(Some(returnValueTemp))

        PlanResult(
          state=returnValueResult.state,
          value=iv.UnitValue // et.Return does not have a value - execution stops
        )
    }  
  }
}
