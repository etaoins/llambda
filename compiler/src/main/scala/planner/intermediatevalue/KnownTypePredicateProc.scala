package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.planner.typecheck.PlanTypeCheck
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

class KnownTypePredicateProc(testingType : vt.SchemeType) extends KnownArtificialProc {
  protected val symbolHint =
    testingType.schemeName
      .replaceAllLiterally("<", "")
      .replaceAllLiterally(">", "") + "?"

  val signature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    hasRestArg=false,
    // We must be able to take any data type without erroring out
    fixedArgs=List(vt.AnySchemeType),
    returnType=Some(vt.CBool),
    attributes=Set()
  )
  
  def planFunction(parentPlan : PlanWriter) : PlannedFunction = {
    // We only have a single argument
    val argumentTemp = ps.CellTemp(ct.DatumCell)
    
    val plan = parentPlan.forkPlan()

    // Perform an inner type check returning a boolean result
    val checkResult = PlanTypeCheck(argumentTemp, vt.AnySchemeType, testingType)(plan)
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
        vt.SatisfiesType(testingType, singleValue.schemeType) match {
          case Some(knownResult) =>
            // We can satisfy this at plan time
            Some(PlanResult(
              state=state,
              value=new ConstantBooleanValue(knownResult)
            ))

          case _ if singleValue.schemeType != vt.AnySchemeType =>
            // We know something about this type
            // Doing an inline check can be a win here

            val cellTemp = singleValue.toTempValue(singleValue.schemeType)
            val checkResult = PlanTypeCheck(
              valueTemp=cellTemp,
              valueType=singleValue.schemeType,
              testingType=testingType
            )

            Some(PlanResult(
              state=state,
              value=checkResult.toIntermediateValue
            ))

          case _ =>
            // We have no type information here
            // We don't gain anything by doing an inline type check
            None
        }

      case _ =>
        None
    }

}
