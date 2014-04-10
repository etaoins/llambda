package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.{et, StorageLocation, SourceLocated}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{InternalCompilerErrorException, ValueNotApplicableException}

private[planner] object PlanExpression {
  // These objects know how to implement certain report procedure directly
  // with plan steps
  private val reportProcPlanners = List(
    reportproc.CadrProcPlanner
  )

  private def lazyPlannedFunction(suggestedName : String, plannedFunction : PlannedFunction)(implicit plan : PlanWriter) : iv.IntermediateValue = {
    // Don't emit this function until its referenced
    val symbolBlock = () => {
      val nativeSymbol = plan.allocProcedureSymbol(suggestedName)
      plan.plannedFunctions += (nativeSymbol -> plannedFunction)

      nativeSymbol
    }

    new iv.KnownProcedure(plannedFunction.signature, symbolBlock, None)
  }

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

      case et.Bind(bindings) =>
        PlanResult(
          state=PlanBind(initialState)(bindings),
          value=iv.UnitValue
        )

      case et.VarRef(storageLoc : StorageLocation) => 
        initialState.values(storageLoc) match {
          case ImmutableValue(value) =>
            // Return the value directly 
            PlanResult(
              state=initialState,
              value=value
            )

          case MutableValue(mutableTemp) =>
            // Load our data pointer
            val recordDataTemp = ps.GcUnmanagedValue()
            plan.steps += ps.StoreRecordLikeData(recordDataTemp, mutableTemp, vt.MutableType)
            
            // Load the data
            val resultTemp = ps.GcManagedValue()
            plan.steps += ps.RecordDataFieldRef(resultTemp, recordDataTemp, vt.MutableType, vt.MutableField)

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
          case MutableValue(mutableTemp) =>
            mutableTemp

          case _ =>
            throw new InternalCompilerErrorException(s"Attempted to mutate non-mutable: ${storageLoc}")
        }
        
        val newValueResult = apply(initialState)(valueExpr)
        val newValueTemp = newValueResult.value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

        // Load our data pointer
        val recordDataTemp = ps.GcUnmanagedValue()
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
          value=lazyPlannedFunction(
            procName,
            PlanRecordTypeConstructor(recordConstructor)
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
          value=lazyPlannedFunction(
            procName,
            PlanRecordTypePredicate(recordPredicate)
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
          value=lazyPlannedFunction(
            procName, 
            PlanRecordTypeAccessor(recordAccessor)
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
          value=lazyPlannedFunction(
            procName,
            PlanRecordTypeMutator(recordMutator)
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

        val innerWriter = plan.forkPlan() 
        val innerValue = apply(postValueState)(innerExpr)(planConfig, innerWriter).value

        // XXX: It'd be nice if we could defer this
        val innerTempType = innerValue.preferredRepresentation
        val innerTemp = innerValue.toTempValue(innerTempType)

        val outerTemp = new ps.TempValue(innerTempType.isGcManaged)
        plan.steps += ps.Parameterize(outerTemp, postValueState.worldPtr,  parameterValueTemps.toList, innerWriter.steps.toList, innerTemp)

        PlanResult(
          state=postValueState,
          value=TempValueToIntermediate(innerTempType, outerTemp)
        )

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
