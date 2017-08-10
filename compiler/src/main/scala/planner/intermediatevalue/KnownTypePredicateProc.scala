package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner._
import llambda.compiler.planner.typecheck._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.InternalCompilerErrorException

class KnownTypePredicateProc(testingType: vt.SchemeType) extends KnownProc(
  TypePredicateProcSignature.toPolymorphic,
  None
) {
  def nativeSymbol(implicit plan: PlanWriter): String =
    SymbolForTypePredicateProc(plan, testingType)

  override def withSelfTemp(selfTemp: ps.TempValue) = {
    // We have no self value so we don't need be to captured and therefore restored
    throw new InternalCompilerErrorException("Attempt to change the self value of a type predicate")
  }

  override def attemptInlineApplication(state: PlannerState)(
      args: List[(ContextLocated, IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] =
    args match {
      case List((_, singleValue)) =>
        val checkResult = PlanTypeCheck.withIntermediateValue(
          checkValue=singleValue,
          testType=testingType
        )

        val resultValue = checkResult.toIntermediateValue

        // Register this result value to enable occurrence typing
        val registeredState = ConstrainValue.addCondAction(state)(
          conditionValue=resultValue,
          ConstrainValue.CondAction(
            subjectValue=singleValue,
            trueConstraint=ConstrainValue.IntersectType(testingType),
            falseConstraint=ConstrainValue.SubtractType(testingType)
          )
        )

        Some(PlanResult(
          state=registeredState,
          value=resultValue
        ))

      case _ =>
        None
    }

}
