package llambda.planner

import collection.mutable

import llambda.{et, StorageLocation, ValueNotApplicableException, ReportProcedure}
import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

private[planner] object PlanExpression {
  // These objects know how to implement certain report procedure directly
  //with plan steps
  private val reportProcPlanners = List(
    reportproc.CadrProcPlanner
  )

  def apply(initialState : PlannerState)(expr : et.Expression, sourceNameHint : Option[String] = None)(implicit planConfig : PlanConfig, plan : PlanWriter) : PlanResult = LocateExceptionsWith(expr) {
    expr match {
      case et.Begin(exprs) =>
        var finalValue : iv.IntermediateValue = iv.UnspecificValue 

        val finalState = exprs.foldLeft(initialState) { case (state, expr) =>
          val result = apply(state)(expr)
          finalValue = result.value

          result.state
        }

        PlanResult(state=finalState, value=finalValue)

      case et.Apply(procExpr, operandExprs) =>
        val procResult = apply(initialState)(procExpr)

        val operandValueBuffer = new mutable.ListBuffer[iv.IntermediateValue]
        val finalState = operandExprs.foldLeft(procResult.state) { case (state, operandExpr) =>
          val operandResult = apply(state)(operandExpr)

          operandValueBuffer += operandResult.value
          operandResult.state
        }

        val operandValues = operandValueBuffer.toList

        val invokableProc = procResult.value.toInvokableProcedure() getOrElse {
          throw new ValueNotApplicableException(expr)
        }

        procResult.value match {
          case knownProc : iv.KnownProcedure if knownProc.reportName.isDefined && planConfig.optimize =>
            val reportName = knownProc.reportName.get

            // Give our reportProcPlanners a chance to plan this more
            // efficiently than a function call
            for(reportProcPlanner <- reportProcPlanners) {
              for(planResult <- reportProcPlanner(finalState)(reportName, operandValues)) {
                // We created an alternative plan; we're done
                return planResult
              }
            }
          
          case other => other
        }

        // Perform a function call
        val applyValueOpt = PlanApplication(invokableProc, operandValues) 

        PlanResult(
          state=finalState,
          value=applyValueOpt.getOrElse(iv.UnspecificValue))

      case et.Bind(bindings) =>
        val finalState = bindings.foldLeft(initialState) { case (state, (storageLoc, initialValue)) =>
          if (planConfig.analysis.mutableVars.contains(storageLoc)) {
            val allocTemp = new ps.TempAllocation
            val mutableTemp = new ps.TempValue
            val variableTemp = new ps.TempValue
            
            val initialValueResult = apply(state)(initialValue)
            val initialValueTemp = initialValueResult.value.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))

            plan.steps += ps.AllocateCells(allocTemp, 1)

            // Create a new mutable
            val recordDataTemp = new ps.TempValue
            plan.steps += ps.RecordInit(mutableTemp, recordDataTemp, allocTemp, 0, vt.MutableCellType)

            // Set the value
            plan.steps += ps.RecordFieldSet(recordDataTemp, vt.MutableCellType, vt.MutableField, initialValueTemp)
            
            initialValueResult.state.withMutable(storageLoc -> mutableTemp)
          }
          else {
            // Send a hint about our name
            val initialValueResult = apply(state)(initialValue, Some(storageLoc.sourceName))

            val reportNamedValue = (initialValueResult.value, storageLoc) match {
              case (knownProc : iv.KnownProcedure, reportProc : ReportProcedure) =>
                // Annotate with our report name so we can optimize when we try
                // to apply this
                // Note this is agnostic to if the implementation is a  native
                // function versus a Scheme  procedures
                knownProc.withReportName(reportProc.reportName)

              case (otherValue, _) =>
                otherValue
            }

            // No planning, just remember this intermediate value
            initialValueResult.state.withImmutable(storageLoc -> reportNamedValue)
          }
        }

        PlanResult(
          state=finalState,
          value=iv.UnspecificValue
        )

      case et.VarRef(storageLoc : StorageLocation) if !planConfig.analysis.mutableVars.contains(storageLoc) =>
        // Return the value directly 
        PlanResult(
          state=initialState,
          value=initialState.immutables(storageLoc)
        )
      
      case et.VarRef(storageLoc) =>
        val mutableTemp = initialState.mutables(storageLoc)
        
        // Load our data pointer
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreRecordCellData(recordDataTemp, mutableTemp, vt.MutableCellType)
        
        // Load the data
        val resultTemp = new ps.TempValue
        plan.steps += ps.RecordFieldRef(resultTemp, recordDataTemp, vt.MutableCellType, vt.MutableField)
        
        // We can be anything here
        val possibleTypes = ct.DatumCell.concreteTypes

        PlanResult(
          state=initialState,
          value=new iv.IntrinsicCellValue(possibleTypes, ct.DatumCell, resultTemp)
        )
      
      case et.MutateVar(storageLoc, valueExpr) =>
        val mutableTemp = initialState.mutables(storageLoc)
        
        val newValueResult = apply(initialState)(valueExpr)
        val newValueTemp = newValueResult.value.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))

        // Load our data pointer
        val recordDataTemp = new ps.TempValue
        plan.steps += ps.StoreRecordCellData(recordDataTemp, mutableTemp, vt.MutableCellType)
        
        // Store the data
        val resultTemp = new ps.TempValue
        plan.steps += ps.RecordFieldSet(recordDataTemp, vt.MutableCellType, vt.MutableField, newValueTemp)

        PlanResult(
          state=newValueResult.state,
          value=iv.UnspecificValue
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
          value=new iv.KnownProcedure(nativeFunc, nativeFunc.nativeSymbol)
        )

      case recordConstructor : et.RecordTypeConstructor =>
        val procName = sourceNameHint.getOrElse {
          // By convention the constructor name is the type name without <>
          recordConstructor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "")
        }

        val nativeSymbol = plan.allocProcedureSymbol(procName)
        val plannedConstructor = PlanRecordTypeConstructor(recordConstructor)

        plan.plannedFunctions += (nativeSymbol -> plannedConstructor)

        PlanResult(
          state=initialState,
          value=new iv.KnownProcedure(plannedConstructor.signature, nativeSymbol)
        )
      
      case recordPredicate : et.RecordTypePredicate =>
        val procName = sourceNameHint.getOrElse {
          // By convention the predicate name is the type name without <> followed by ?
          recordPredicate.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + "?"
        }

        val nativeSymbol = plan.allocProcedureSymbol(procName)
        val plannedPredicate = PlanRecordTypePredicate(recordPredicate)

        plan.plannedFunctions += (nativeSymbol -> plannedPredicate)

        PlanResult(
          state=initialState,
          value=new iv.KnownProcedure(plannedPredicate.signature, nativeSymbol)
        )
      
      case recordAccessor : et.RecordTypeAccessor =>
        val procName = sourceNameHint.getOrElse {
          // By convention the accessor name is "{constructorName}-{fieldName}"
          recordAccessor.recordType.sourceName
            .replaceAllLiterally("<", "")
            .replaceAllLiterally(">", "") + 
            "-" + recordAccessor.field.sourceName
        }

        val nativeSymbol = plan.allocProcedureSymbol(procName)
        val plannedAccessor = PlanRecordTypeAccessor(recordAccessor)

        plan.plannedFunctions += (nativeSymbol -> plannedAccessor)

        PlanResult(
          state=initialState,
          value=new iv.KnownProcedure(plannedAccessor.signature, nativeSymbol)
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

        val nativeSymbol = plan.allocProcedureSymbol(procName)
        val plannedMutator = PlanRecordTypeMutator(recordMutator)

        plan.plannedFunctions += (nativeSymbol -> plannedMutator)

        PlanResult(
          state=initialState,
          value=new iv.KnownProcedure(plannedMutator.signature, nativeSymbol)
        )

      case et.Cast(valueExpr, targetType) =>
        val valueResult = apply(initialState)(valueExpr)

        val castTemp = valueResult.value.toRequiredTempValue(targetType)
        val castValue = TempValueToIntermediate(targetType, castTemp)
          
        PlanResult(state=valueResult.state, value=castValue)
    }  
  }
}
