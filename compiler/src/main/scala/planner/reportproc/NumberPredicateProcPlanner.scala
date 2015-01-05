package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.typecheck.PlanTypeCheck
import llambda.compiler.{valuetype => vt}

object NumberPredicateProcPlanner extends ReportProcPlanner {
  private def numberTypePredicate(state : PlannerState)(
    arg : (ContextLocated, iv.IntermediateValue), testingType : vt.SchemeType
  )(implicit plan : PlanWriter) : PlanResult = {
    val argValue = arg._2

    plan.withContextLocation(arg._1) {
      argValue.castToSchemeType(vt.NumberType)
    }

    // We now know the above is definitely a number
    val numberConstraintedType = argValue.schemeType & vt.NumberType

    val checkResult = PlanTypeCheck(
      checkValue={argValue.toBoxedValue()},
      valueType=numberConstraintedType,
      testType=testingType
    )

    val resultValue = checkResult.toIntermediateValue

    // Register this result value to enable occurrence typing
    val registeredState = ConstrainType.addCondAction(state)(
      conditionValue=resultValue,
      ConstrainType.CondAction(
        subjectValue=argValue,
        trueConstraint=ConstrainType.IntersectType(testingType),
        falseConstraint=ConstrainType.SubtractType(testingType)
      )
    )

    PlanResult(
      state=registeredState,
      values=SingleValue(resultValue)
    )
  }

  override def planWithResult(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[PlanResult] = (reportName, args) match {
    case ("inexact?", List(singleValue)) =>
      Some(numberTypePredicate(state)(singleValue, vt.FlonumType))
    
    case (_, List(singleValue)) if Set("exact?", "exact-integer?").contains(reportName) =>
      Some(numberTypePredicate(state)(singleValue, vt.ExactIntegerType))

    case _ =>
      None
  }
}
