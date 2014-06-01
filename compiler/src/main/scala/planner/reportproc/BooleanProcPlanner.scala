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
    case ("boolean?", List((_, singleOperand))) =>
      predicatePlanner(state)(singleOperand, ct.BooleanCell)
    
    case ("not", List((_, value)))  =>
      val truthyPredTemp = value.toTruthyPredicate()

      // Create a literal 0
      val zeroTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.CreateNativeInteger(zeroTemp, 0, 1)

      val negatedTemp = ps.Temp(vt.Predicate)
      plan.steps += ps.IntegerCompare(negatedTemp, ps.CompareCond.Equal, None, truthyPredTemp, zeroTemp)

      Some(PlanResult(
        state=state,
        value=new iv.NativePredicateValue(negatedTemp)
      ))

    case _ =>
      None
  }
}
