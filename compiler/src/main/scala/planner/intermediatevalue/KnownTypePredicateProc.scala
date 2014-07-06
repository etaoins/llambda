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

          case _ =>
            None
        }

      case _ =>
        None
    }

}
