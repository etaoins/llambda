package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.{et, StorageLocation, SourceLocated, RuntimeErrorMessage}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{InternalCompilerErrorException, ValueNotApplicableException}

private[planner] object PlanExpression {
  // These objects know how to implement certain report procedure directly
  // with plan steps
  private val reportProcPlanners = List[reportproc.ReportProcPlanner](
    reportproc.BooleanProcPlanner,
    reportproc.CadrProcPlanner,
    reportproc.EquivalenceProcPlanner,
    reportproc.NumberProcPlanner
  )

  def apply(initialState : PlannerState)(expr : et.Expression, sourceNameHint : Option[String] = None)(implicit planConfig : PlanConfig, plan : PlanWriter) : PlanResult = LocateExceptionsWith(expr) {
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

        val operandBuffer = new mutable.ListBuffer[(SourceLocated, iv.IntermediateValue)]

        val finalState = operandExprs.foldLeft(procResult.state) { case (state, operandExpr) =>
          val operandResult = apply(state)(operandExpr)

          operandBuffer += ((operandExpr, operandResult.value))
          operandResult.state
        }

        val operands = operandBuffer.toList

        val invokableProc = procResult.value.toInvokableProcedure() getOrElse {
          throw new ValueNotApplicableException(expr)
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

      case et.TopLevelDefinition(bindings) =>
        PlanResult(
          state=PlanBind(initialState)(bindings),
          value=iv.UnitValue
        )

      case et.InternalDefinition(bindings, bodyExpr) =>
        val bodyState = PlanBind(initialState)(bindings)

        // Return to our initial state
        // InternalDefinitions can re-bind existing variables and they must be restored after its body is planned
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

          case MutableValue(mutableTemp, needsUndefCheck) =>
            // Load our data pointer
            val recordDataTemp = ps.RecordLikeDataTemp()
            plan.steps += ps.StoreRecordLikeData(recordDataTemp, mutableTemp, vt.MutableType)
            
            // Load the data
            val resultTemp = ps.CellTemp(ct.DatumCell)
            plan.steps += ps.RecordDataFieldRef(resultTemp, recordDataTemp, vt.MutableType, vt.MutableField)

            if (needsUndefCheck) {
              val errorMessage = RuntimeErrorMessage("accessUndefined", "Recursively defined value referenced before its initialization") 
              plan.steps += ps.AssertRecordDataFieldDefined(worldPtr, resultTemp, vt.MutableField, errorMessage)
            }

            // Dispose of our data pointer
            plan.steps += ps.DisposeValue(recordDataTemp)
            
            // We can be anything here
            val possibleTypes = ct.DatumCell.concreteTypes

            PlanResult(
              state=initialState,
              value=new iv.IntrinsicCellValue(possibleTypes, ct.DatumCell, resultTemp)
            )
        }
      
      case et.MutateVar(storageLoc, valueExpr) =>
        val mutableTemp = initialState.values(storageLoc) match {
          case MutableValue(mutableTemp, _) =>
            mutableTemp

          case _ =>
            throw new InternalCompilerErrorException(s"Attempted to mutate non-mutable: ${storageLoc}")
        }
        
        val newValueResult = apply(initialState)(valueExpr)
        val newValueTemp = newValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

        // Load our data pointer
        val recordDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.StoreRecordLikeData(recordDataTemp, mutableTemp, vt.MutableType)
        
        // Store the data
        plan.steps += ps.RecordDataFieldSet(recordDataTemp, vt.MutableType, vt.MutableField, newValueTemp)

        // Dispose of the data pointer
        plan.steps += ps.DisposeValue(recordDataTemp)

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
          value=new iv.KnownProcedure(nativeFunc.signature, () => nativeFunc.nativeSymbol, None)
        )

      case recordConstructor : et.RecordTypeConstructor =>
        val procName = sourceNameHint.getOrElse {
          // By convention the constructor name is the type name without <>
          recordConstructor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "")
        }

        PlanResult(
          state=initialState,
          value=LazyPlannedFunction(
            suggestedName=procName,
            plannedFunction=PlanRecordTypeConstructor(recordConstructor),
            selfTempOpt=None
          )
        )
      
      case recordPredicate : et.RecordTypePredicate =>
        val procName = sourceNameHint.getOrElse {
          // By convention the predicate name is the type name without <> followed by ?
          recordPredicate.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + "?"
        }

        PlanResult(
          state=initialState,
          value=LazyPlannedFunction(
            suggestedName=procName,
            plannedFunction=PlanRecordTypePredicate(recordPredicate),
            selfTempOpt=None
          )
        )
      
      case recordAccessor : et.RecordTypeAccessor =>
        val procName = sourceNameHint.getOrElse {
          // By convention the accessor name is "{constructorName}-{fieldName}"
          recordAccessor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + 
            "-" + recordAccessor.field.sourceName
        }

        PlanResult(
          state=initialState,
          value=LazyPlannedFunction(
            suggestedName=procName, 
            plannedFunction=PlanRecordTypeAccessor(recordAccessor),
            selfTempOpt=None
          )
        )
      
      case recordMutator : et.RecordTypeMutator =>
        val procName = sourceNameHint.getOrElse {
          // By convention the mutatorName name is "set-{constructorName}-{fieldName}!"
          "set-" +
            recordMutator.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + 
            "-" + recordMutator.field.sourceName +
            "!"
        }

        PlanResult(
          state=initialState,
          value=LazyPlannedFunction(
            suggestedName=procName,
            plannedFunction=PlanRecordTypeMutator(recordMutator),
            selfTempOpt=None
          )
        )

      case et.Cast(valueExpr, targetType) =>
        val valueResult = apply(initialState)(valueExpr)

        val castTemp = valueResult.value.toTempValue(targetType)
        val castValue = TempValueToIntermediate(targetType, castTemp)
          
        PlanResult(state=valueResult.state, value=castValue)

      case et.Lambda(fixedArgs, restArg, body) =>
        PlanLambda(initialState, plan)(fixedArgs, restArg, body, sourceNameHint)

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
        val postInnerResult = PlanExpression(postValueState)(innerExpr)
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
