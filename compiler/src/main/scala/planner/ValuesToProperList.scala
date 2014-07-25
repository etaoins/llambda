package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

private[planner] object ValuesToProperList {
  /** Converts a list of intermediate values to an intermediate value representing a proper list
    *
    * This automatically takes advantage of immutable pair support to build constant lists when the Scheme dialect
    * allows it
    */
  def apply(memberValues : List[iv.IntermediateValue])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = 
    memberValues match {
      case carValue :: restValues =>
        // Recurse down our cdr
        val cdrValue = apply(restValues)
        ValuesToPair(carValue, cdrValue, Some(restValues.length + 1))

      case Nil =>
        // No more list values
        iv.EmptyListValue
    }
}
