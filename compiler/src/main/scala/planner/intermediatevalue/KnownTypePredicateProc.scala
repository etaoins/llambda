package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.planner.typecheck._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

class KnownTypePredicateProc(testingType : vt.SchemeType) extends KnownArtificialProc {
  protected val symbolHint =
    vt.NameForType(testingType)
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + "?"

  val signature = TypePredicateProcSignature
  
  def planFunction(parentPlan : PlanWriter, allocedSymbol : String) : PlannedFunction = {
    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.AnyCell)
    
    val plan = parentPlan.forkPlan()

    // Perform an inner type check returning a boolean result
    // Note that this is forced inline check because we pass selfSymbolOpt. The normal PlanTypeCheck entry point to the
    // type system might decide  to call this type check out-of-line which won't work because *this* is the out-of-line
    // implementation.
    val cellTemp = BoxedValue(ct.AnyCell, argumentTemp)
    val checkResult = PlanTypeCheck(cellTemp, vt.AnySchemeType, testingType, selfSymbolOpt=Some(allocedSymbol))(plan)

    val retValueTemp = checkResult.toNativePred()(plan)

    plan.steps += ps.Return(Some(retValueTemp))

    PlannedFunction(
      signature=signature,
      namedArguments=List(("value" -> argumentTemp)),
      steps=plan.steps.toList,
      worldPtrOpt=None,
      debugContextOpt=None
    )
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] =
    operands match {
      case List((_, singleValue)) =>
        val checkResult = PlanTypeCheck(
          checkValue={singleValue.toBoxedValue()},
          valueType=singleValue.schemeType,
          testType=testingType
        )

        val resultValue = checkResult.toIntermediateValue

        // Register this result value to enable occurrence typing
        val registeredState = ConstrainType.addCondAction(state)(
          conditionValue=resultValue,
          ConstrainType.CondAction(
            subjectValue=singleValue,
            trueConstraint=ConstrainType.IntersectType(testingType),
            falseConstraint=ConstrainType.SubtractType(testingType)
          )
        )

        Some(PlanResult(
          state=registeredState,
          values=SingleValue(resultValue)
        ))

      case _ =>
        None
    }

}
