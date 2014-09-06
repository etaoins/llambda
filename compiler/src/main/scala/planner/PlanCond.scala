package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

object PlanCond {
  /** Modifies a state to include appropriate type constraints for the passed branch values
    *
    * If one of the branches of the condition is definitely false we can infer quite a lot of information if
    * the condition's result value is true-y:
    * 1) The test expression was either true or false, depending which branch is the #f is in
    * 2) The value is the branch that wasn't #f is true-y
    * 3) Any additional actions based on the value of that branch being true should be applied
    *
    * This might seem academic but (and) is implemented as (if cond1 cond2 #f). All three of the above inferences 
    * are required for (and) to interact properly with occurrence typing. Although only the case with #f being in
    * the false branch is used by (and) the other case is included for symmetry
    */
  private def constrainIntermediateValues(resultState : PlannerState)(
    testValue : iv.IntermediateValue,
    trueState : PlannerState,
    trueValue : iv.IntermediateValue,
    falseState : PlannerState,
    falseValue : iv.IntermediateValue,
    resultValue : iv.IntermediateValue
  ) : PlannerState = {
    import ConstrainType._

    val condActions = if (vt.SatisfiesType(vt.ConstantBooleanType(false), trueValue.schemeType) == Some(true)) {
      List(
        // If (if testExpr #f falseExpr) is not false then testExpr was false
        CondAction(
          subjectValue=resultValue,
          trueConstraint=IntersectType(vt.ConstantBooleanType(false)),
          falseConstraint=SubtractType(vt.ConstantBooleanType(false))
        ),
        // If (if testExpr #f falseExpr) is not false then falseExpr was truthy
        CondAction(
          subjectValue=falseValue,
          trueConstraint=SubtractType(vt.ConstantBooleanType(false)),
          falseConstraint=IntersectType(vt.ConstantBooleanType(false))
        )
      ) ++
      condActionsForConditionValue(falseState)(falseValue).map { condAction =>
        // If (if testExpr #f falseExpr) is not false then falseExpr's true actions should be run
        condAction.copy(falseConstraint=PreserveType)
      }
    }
    else if (vt.SatisfiesType(vt.ConstantBooleanType(false), falseValue.schemeType) == Some(true)) {
      List(
        // If (if testExpr 'something #f) is not false then testExpr was truthy
        CondAction(
          subjectValue=testValue,
          trueConstraint=SubtractType(vt.ConstantBooleanType(false)),
          falseConstraint=IntersectType(vt.ConstantBooleanType(false))
        ),
        // If (if testExpr trueExpr #f) is not false then trueExpr was truthy
        CondAction(
          subjectValue=trueValue,
          trueConstraint=SubtractType(vt.ConstantBooleanType(false)),
          falseConstraint=IntersectType(vt.ConstantBooleanType(false))
        )
      ) ++
      condActionsForConditionValue(trueState)(trueValue).map { condAction =>
        // If (if testExpr trueExpr #f) is not false then trueExpr's true actions should be run
        condAction.copy(falseConstraint=PreserveType)
      }
    }
    else {
      Nil
    }

    condActions.foldLeft(resultState) { case (state, condAction) =>
      ConstrainType.addCondAction(state)(resultValue, condAction)
    }
  }

  // Wrapper for constrainIntermediateValues
  private def constrainResultValues(resultState : PlannerState)(
    testValue : iv.IntermediateValue,
    trueState : PlannerState,
    trueValues : ResultValues,
    falseState : PlannerState,
    falseValues : ResultValues,
    resultValues : ResultValues
  ) : PlannerState = (trueValues, falseValues, resultValues) match {
    case (SingleValue(trueValue), SingleValue(falseValue), SingleValue(resultValue)) =>
      constrainIntermediateValues(resultState)(testValue, trueState, trueValue, falseState, falseValue, resultValue)

    case _ =>
      resultState
  }

  def apply(initialState : PlannerState)(
      testExpr : et.Expr,
      trueExpr : et.Expr,
      falseExpr : et.Expr
  )(implicit plan : PlanWriter) : PlanResult = {
    implicit val worldPtr = initialState.worldPtr

    val testResult = PlanExpr(initialState)(testExpr)
    val testValue = testResult.values.toIntermediateValue()

    vt.SatisfiesType(vt.ConstantBooleanType(false), testValue.schemeType) match {
      case Some(true) =>
        // The test result must be false
        PlanExpr(testResult.state)(falseExpr)

      case Some(false) =>
        // The test result can't be false
        PlanExpr(testResult.state)(trueExpr)

      case None =>
        val truthyPred = testValue.toTempValue(vt.Predicate)

        val trueWriter = plan.forkPlan()
        // The test expression is definitely not false in this branch
        val trueConstraint = ConstrainType.SubtractType(vt.ConstantBooleanType(false))
        val initialTrueState = ConstrainType(testResult.state)(testValue, trueConstraint)(plan.config)
        val trueResult = PlanExpr(initialTrueState)(trueExpr)(trueWriter)
        val trueValues = trueResult.values

        val falseWriter = plan.forkPlan() 
        // The test expression is definitely false in this branch
        val falseConstraint = ConstrainType.IntersectType(vt.ConstantBooleanType(false))
        val initialFalseState = ConstrainType(testResult.state)(testValue, falseConstraint)(plan.config)
        val falseResult = PlanExpr(initialFalseState)(falseExpr)(falseWriter)
        val falseValues = falseResult.values
    
        val planPhiResult = PlanResultValuesPhi(trueWriter, trueValues, falseWriter, falseValues)
        val resultValues = planPhiResult.resultValues

        plan.steps += ps.CondBranch(
          planPhiResult.resultTemp,
          truthyPred,
          trueWriter.steps.toList, planPhiResult.leftTempValue,
          falseWriter.steps.toList, planPhiResult.rightTempValue
        )
        val constrainedState = constrainResultValues(testResult.state)(
          testValue=testValue,
          trueState=trueResult.state,
          trueValues=trueValues,
          falseState=falseResult.state,
          falseValues=falseValues,
          resultValues=resultValues
        )

        PlanResult(
          state=constrainedState,
          values=resultValues
        )
    }
  }
}
