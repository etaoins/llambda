package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

object PlanCond {
  def apply(initialState : PlannerState)(
      testExpr : et.Expr,
      trueExpr : et.Expr,
      falseExpr : et.Expr
  )(implicit plan : PlanWriter) : PlanResult = {
    implicit val worldPtr = initialState.worldPtr

    val testResult = PlanExpr(initialState)(testExpr)

    vt.SatisfiesType(vt.ConstantBooleanType(false), testResult.value.schemeType) match {
      case Some(true) =>
        // The test result must be false
        PlanExpr(testResult.state)(falseExpr)

      case Some(false) =>
        // The test result can't be false
        PlanExpr(testResult.state)(trueExpr)

      case None =>
        val testValue = testResult.value
        val truthyPred = testValue.toTempValue(vt.Predicate)

        val trueWriter = plan.forkPlan()
        // The test expression is definitely not false in this branch
        val trueConstraint = ConstrainType.SubtractType(vt.ConstantBooleanType(false))
        val initialTrueState = ConstrainType(testResult.state)(testResult.value, trueConstraint)(plan.config)
        val trueResult = PlanExpr(initialTrueState)(trueExpr)(trueWriter)
        val trueValue = trueResult.value

        val falseWriter = plan.forkPlan() 
        // The test expression is definitely false in this branch
        val falseConstraint = ConstrainType.IntersectType(vt.ConstantBooleanType(false))
        val initialFalseState = ConstrainType(testResult.state)(testResult.value, falseConstraint)(plan.config)
        val falseResult = PlanExpr(initialFalseState)(falseExpr)(falseWriter)
        val falseValue = falseResult.value
    
        val planPhiResult = trueValue.planPhiWith(falseValue)(trueWriter, falseWriter)
        val resultValue = planPhiResult.resultIntermediate

        plan.steps += ps.CondBranch(
          planPhiResult.resultTemp,
          truthyPred,
          trueWriter.steps.toList, planPhiResult.ourTempValue,
          falseWriter.steps.toList, planPhiResult.theirTempValue
        )

        // If one of the branches of the condition is definitely false we can infer quite a lot of information if
        // the condition's result value is true-y:
        // 1) The test expression was either true or false, depending which branch is the #f is in
        // 2) The value is the branch that wasn't #f is true-y
        // 3) Any additional actions based on the value of that branch being true should be applied
        //
        // This might seem academic but (and) is implemented as (if cond1 cond2 #f). All three of the above inferences 
        // are required for (and) to interact properly with occurrence typing. Although only the case with #f being in
        // the false branch is used by (and) the other case is included for symmetry
        val condActions = if (vt.SatisfiesType(vt.ConstantBooleanType(false), trueValue.schemeType) == Some(true)) {
          import ConstrainType._

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
          condActionsForConditionValue(falseResult.state)(falseValue).map { condAction =>
            // If (if testExpr #f falseExpr) is not false then falseExpr's true actions should be run
            condAction.copy(falseConstraint=PreserveType)
          }
        }
        else if (vt.SatisfiesType(vt.ConstantBooleanType(false), falseValue.schemeType) == Some(true)) {
          import ConstrainType._

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
          condActionsForConditionValue(trueResult.state)(trueValue).map { condAction =>
            // If (if testExpr trueExpr #f) is not false then trueExpr's true actions should be run
            condAction.copy(falseConstraint=PreserveType)
          }
        }
        else {
          Nil
        }

        val constrainedState = condActions.foldLeft(testResult.state) { case (state, condAction) =>
          ConstrainType.addCondAction(state)(resultValue, condAction)
        }

        PlanResult(
          state=constrainedState,
          value=resultValue
        )
    }
  }
}
