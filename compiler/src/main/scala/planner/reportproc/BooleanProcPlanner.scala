package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object BooleanProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("not", List((_, value)))  =>
      val truthyPredTemp = value.toTruthyPredicate()

      // Store literal 0
      val zeroTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.StoreNativeInteger(zeroTemp, 0, 1)

      val negatedTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.IntegerCompare(negatedTemp, truthyPredTemp, zeroTemp)

      Some(PlanResult(
        state=state,
        value=new iv.NativePredicateValue(negatedTemp)
      ))

    case _ =>
      None
  }
}
