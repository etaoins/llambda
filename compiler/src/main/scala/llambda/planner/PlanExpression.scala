package llambda.planner

import collection.mutable

import llambda.nfi
import llambda.{et, StorageLocation, InternalCompilerErrorException}
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}
import llambda.analyzer.AnalysisResult

private[planner] object PlanExpression {
  def apply(initialState : PlannerState)(expr : et.Expression)(implicit analysis : AnalysisResult, planSteps : StepBuffer) : PlanResult = LocateExceptionsWith(expr) {
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

        val operandValues = new mutable.ListBuffer[iv.IntermediateValue]
        val finalState = operandExprs.foldLeft(procResult.state) { case (state, operandExpr) =>
          val operandResult = apply(state)(operandExpr)

          operandValues += operandResult.value
          operandResult.state
        }

        val invokableProc = procResult.value match {
          case invokable : InvokableProcedure =>
            invokable
          case _ =>
            throw new InternalCompilerErrorException("Attempted to invoke non-invokable value")
        }

        val applyValueOpt = PlanApplication(invokableProc, operandValues.toList) 

        PlanResult(
          state=finalState,
          value=applyValueOpt.getOrElse(iv.UnspecificValue))

      case et.Bind(bindings) =>
        val finalState = bindings.foldLeft(initialState) { case (state, (storageLoc, initialValue)) =>
          val initialValueResult = apply(state)(initialValue)

          if (analysis.mutableVars.contains(storageLoc)) {
            val allocTemp = new ps.TempAllocation
            val mutableTemp = new ps.TempValue
            val variableTemp = new ps.TempValue
            
            val initialValueTemp = initialValueResult.value.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))

            planSteps += ps.AllocateCons(allocTemp, 1)
            planSteps += ps.MutableVarInit(mutableTemp, allocTemp, 0)
            planSteps += ps.MutableVarSet(mutableTemp, initialValueTemp)
            
            initialValueResult.state.withMutable(storageLoc -> mutableTemp)
          }
          else {
            // No planning, just remember this intermediate value
            initialValueResult.state.withImmutable(storageLoc -> initialValueResult.value)
          }
        }

        PlanResult(
          state=finalState,
          value=iv.UnspecificValue
        )

      case et.VarRef(storageLoc : StorageLocation) if !analysis.mutableVars.contains(storageLoc) =>
        // Return the value directly 
        PlanResult(
          state=initialState,
          value=initialState.immutables(storageLoc)
        )
      
      case et.VarRef(storageLoc : StorageLocation) =>
        val mutableTemp = initialState.mutables(storageLoc)
        val resultTemp = new ps.TempValue

        planSteps += ps.MutableVarRef(resultTemp, mutableTemp)
        
        // We can be anything here
        val possibleTypes = bt.BoxedDatum.concreteTypes

        PlanResult(
          state=initialState,
          value=new iv.DynamicBoxedValue(possibleTypes, bt.BoxedDatum, resultTemp)
        )
      
      case et.VarRef(_) =>
        throw new InternalCompilerErrorException("Non-storage variable reference leaked to planner!")

      case et.MutateVar(storageLoc, valueExpr) =>
        val mutableTemp = initialState.mutables(storageLoc)
        
        val newValueResult = apply(initialState)(valueExpr)
        val newValueTemp = newValueResult.value.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))

        planSteps += ps.MutableVarSet(mutableTemp, newValueTemp)

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

        val trueSteps = new StepBuffer
        val trueValue = apply(testResult.state)(trueExpr)(analysis, trueSteps).value

        val falseSteps = new StepBuffer
        val falseValue = apply(testResult.state)(falseExpr)(analysis, falseSteps).value
    
        val planPhiResult = trueValue.planPhiWith(falseValue)(trueSteps, falseSteps)

        planSteps += ps.CondBranch(
          planPhiResult.resultTemp,
          truthyPred,
          trueSteps.toList, planPhiResult.ourTempValue,
          falseSteps.toList, planPhiResult.theirTempValue
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
