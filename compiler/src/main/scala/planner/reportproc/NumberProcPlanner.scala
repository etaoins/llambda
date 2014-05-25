package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object NumberProcPlanner extends ReportProcPlanner {
  private def compareOperands(state : PlannerState)(compareCond : ps.CompareCond.CompareCond, val1 : iv.IntermediateValue, val2 : iv.IntermediateValue)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if ((val1.possibleTypes == Set(ct.ExactIntegerCell)) &&
        (val2.possibleTypes == Set(ct.ExactIntegerCell)))
    {
      // Do a direct integer comparison
      val val1Temp = val1.toTempValue(vt.Int64)
      val val2Temp = val2.toTempValue(vt.Int64)

      val predicateTemp = ps.Temp(vt.Predicate)

      val signed = if (compareCond == ps.CompareCond.Equal) {
        None
      }
      else {
        Some(true)
      }

      // Do a direct integer compare
      plan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

      Some(PlanResult(
        state=state,
        value=new iv.NativePredicateValue(predicateTemp)
      ))
    }
    else {
      None
    }
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.Equal, val1, val2)
    
    case (">", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThan, val1, val2)
    
    case (">=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThanEqual, val1, val2)
    
    case ("<", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThan, val1, val2)
    
    case ("<=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThanEqual, val1, val2)

    case _ =>
      None
  }
}
