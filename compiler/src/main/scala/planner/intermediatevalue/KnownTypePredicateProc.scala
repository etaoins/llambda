package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownTypePredicateProc(signature : ProcedureSignature, nativeSymbol : String, val testingType : vt.SchemeType) extends KnownProc(signature, nativeSymbol, None) {
  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownTypePredicateProc(signature, nativeSymbol, testingType)
  }

  override def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] =
    operands match {
      case List((_, singleValue)) =>
        singleValue.schemeType.satisfiesType(testingType) match {
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
            val resultPredIr = PlanTypeCheck(
              valueTemp=cellTemp,
              valueType=singleValue.schemeType,
              testingType=testingType
            )

            Some(PlanResult(
              state=state,
              value=new NativePredicateValue(resultPredIr)
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
