package llambda.planner

import collection.mutable

import llambda.nfi
import llambda.{et, StorageLocation, ValueNotApplicableException, ReportProcedure}
import llambda.{boxedtype => bt}
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
            val initialValueTemp = initialValueResult.value.toRequiredTempValue(vt.BoxedValue(bt.BoxedDatum))

            plan.steps += ps.AllocateCons(allocTemp, 1)
            plan.steps += ps.MutableVarInit(mutableTemp, allocTemp, 0)
            plan.steps += ps.MutableVarSet(mutableTemp, initialValueTemp)
            
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
        val resultTemp = new ps.TempValue

        plan.steps += ps.MutableVarRef(resultTemp, mutableTemp)
        
        // We can be anything here
        val possibleTypes = bt.BoxedDatum.concreteTypes

        PlanResult(
          state=initialState,
          value=new iv.DynamicBoxedValue(possibleTypes, bt.BoxedDatum, resultTemp)
        )
      
      case et.MutateVar(storageLoc, valueExpr) =>
        val mutableTemp = initialState.mutables(storageLoc)
        
        val newValueResult = apply(initialState)(valueExpr)
        val newValueTemp = newValueResult.value.toRequiredTempValue(vt.BoxedValue(bt.BoxedDatum))

        plan.steps += ps.MutableVarSet(mutableTemp, newValueTemp)

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
    }
  }
}
