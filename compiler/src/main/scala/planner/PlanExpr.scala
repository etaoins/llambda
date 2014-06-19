package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.{et, StorageLocation, ContextLocated, RuntimeErrorMessage}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{InternalCompilerErrorException, ValueNotApplicableException}

private[planner] object PlanExpr {
  // These objects know how to implement certain report procedure directly
  // with plan steps
  private val reportProcPlanners = List[reportproc.ReportProcPlanner](
    reportproc.ApplyProcPlanner,
    reportproc.BooleanProcPlanner,
    reportproc.CadrProcPlanner,
    reportproc.EquivalenceProcPlanner,
    reportproc.ListProcPlanner,
    reportproc.NumberProcPlanner,
    reportproc.VectorProcPlanner
  )

  private def resultWithFunction(state : PlannerState)(suggestedName : String, plannedFunction : PlannedFunction)(implicit plan : PlanWriter) : PlanResult = {
    val nativeSymbol = plan.allocProcedureSymbol(suggestedName)

    plan.plannedFunctions += (nativeSymbol -> plannedFunction)

    PlanResult(
      state=state,
      value=new iv.KnownProcedure(
        signature=plannedFunction.signature,
        nativeSymbol=nativeSymbol,
        selfTempOpt=None
      )
    )
  }
  
  def apply(initialState : PlannerState)(expr : et.Expr, sourceNameHint : Option[String] = None)(implicit planConfig : PlanConfig, plan : PlanWriter) : PlanResult = plan.withContextLocation(expr) {
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

      case et.Apply(procExpr, operandExprs) =>
        val procResult = apply(initialState)(procExpr)

        val operandBuffer = new mutable.ListBuffer[(ContextLocated, iv.IntermediateValue)]

        val finalState = operandExprs.foldLeft(procResult.state) { case (state, operandExpr) =>
          val operandResult = apply(state)(operandExpr)

          operandBuffer += ((operandExpr, operandResult.value))
          operandResult.state
        }

        val operands = operandBuffer.toList

        val invokableProc = procResult.value.toInvokableProcedure() getOrElse {
          throw new ValueNotApplicableException(expr, procResult.value.typeDescription)
        }

        procResult.value match {
          case knownProc : iv.KnownProcedure if knownProc.reportName.isDefined && planConfig.optimize =>
            val reportName = knownProc.reportName.get

            // Give our reportProcPlanners a chance to plan this more
            // efficiently than a function call
            for(reportProcPlanner <- reportProcPlanners) {
              for(planResult <- reportProcPlanner(finalState)(reportName, operands)) {
                // We created an alternative plan; we're done
                return planResult
              }
            }
          
          case other => other
        }

        // Perform a function call
        val applyValueOpt = PlanApplication(invokableProc, operands) 

        PlanResult(
          state=finalState,
          value=applyValueOpt.getOrElse(iv.UnitValue))

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
        val truthyPred = testResult.value.toTruthyPredicate()

        val trueWriter = plan.forkPlan()
        val trueValue = apply(testResult.state)(trueExpr)(planConfig, trueWriter).value

        val falseWriter = plan.forkPlan() 
        val falseValue = apply(testResult.state)(falseExpr)(planConfig, falseWriter).value
    
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
      
      case nativeFunc : et.NativeFunction =>
        PlanResult(
          state=initialState,
          value=new iv.KnownProcedure(nativeFunc.signature, nativeFunc.nativeSymbol, None)
        )

      case recordConstructor : et.RecordTypeConstructor =>
        val plannedFunction = PlanRecordTypeConstructor(recordConstructor)

        val procName = sourceNameHint.getOrElse {
          // By convention the constructor name is the type name without <>
          recordConstructor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "")
        }

        resultWithFunction(initialState)(procName, plannedFunction)
      
      case recordPredicate : et.RecordTypePredicate =>
        val plannedFunction = PlanRecordTypePredicate(recordPredicate)

        val procName = sourceNameHint.getOrElse {
          // By convention the predicate name is the type name without <> followed by ?
          recordPredicate.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + "?"
        }

        resultWithFunction(initialState)(procName, plannedFunction)
      
      case recordAccessor : et.RecordTypeAccessor =>
        val plannedFunction = PlanRecordTypeAccessor(recordAccessor)

        val procName = sourceNameHint.getOrElse {
          // By convention the accessor name is "{constructorName}-{fieldName}"
          recordAccessor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + 
            "-" + recordAccessor.field.sourceName
        }
        
        resultWithFunction(initialState)(procName, plannedFunction)
      
      case recordMutator : et.RecordTypeMutator =>
        val plannedFunction = PlanRecordTypeMutator(recordMutator)

        val procName = sourceNameHint.getOrElse {
          // By convention the mutatorName name is "set-{constructorName}-{fieldName}!"
          "set-" +
            recordMutator.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + 
            "-" + recordMutator.field.sourceName +
            "!"
        }

        resultWithFunction(initialState)(procName, plannedFunction)

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

          val parameterTemp = parameterResult.value.toTempValue(vt.IntrinsicCellType(ct.ProcedureCell))
          val valueTemp = valueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

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
        val returnValueTemp = returnValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

        plan.steps += ps.Return(Some(returnValueTemp))

        PlanResult(
          state=returnValueResult.state,
          value=iv.UnitValue // et.Return does not have a value - execution stops
        )
    }  
  }
}
