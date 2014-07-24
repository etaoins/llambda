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
    testingType.schemeName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + "?"

  val signature = TypePredicateProcSignature
  
  def planFunction(parentPlan : PlanWriter) : PlannedFunction = {
    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.DatumCell)
    
    val plan = parentPlan.forkPlan()

    // Perform an inner type check returning a boolean result
    // Note that this is forced inline chck. The normal PlanTypeCheck entry point to the type system might decide
    // to call this type check out-of-line which won't work because *this* is the out-of-line implementation.
    val cellTemp = BoxedValue(ct.DatumCell, argumentTemp)
    val checkResult = PlanTypeCheck(cellTemp, vt.AnySchemeType, testingType, mustInline=true)(plan)

    val retValueTemp = checkResult.toNativeCBool()(plan)

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

        Some(PlanResult(
          state=state,
          value=checkResult.toIntermediateValue
        ))

      case _ =>
        None
    }

}
