package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.planner.{intermediatevalue => iv}

object TypePredicateProcForType {
  /** Returns the type predicate known value for the given type
    *
    * Only one type predicate procedure will be planned for each type
    */
  def apply(schemeType : vt.SchemeType)(implicit plan : PlanWriter) : iv.KnownTypePredicateProc =
    plan.plannedTypePredicates.getOrElseUpdate(schemeType, {
      new iv.KnownTypePredicateProc(testingType=schemeType)
    })
}
