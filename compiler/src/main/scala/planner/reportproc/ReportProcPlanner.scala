package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.SourceLocated
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Optionally replaces a call to a report procedure with plan steps */
trait ReportProcPlanner {
  protected def predicatePlanner(initialState : PlannerState)(value : iv.IntermediateValue, expectedType : ct.CellType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    val concreteTypes = expectedType.concreteTypes

    if (value.possibleTypes.subsetOf(concreteTypes)) {
      // Must be this type
      Some(PlanResult(
        state=initialState,
        value=new iv.ConstantBooleanValue(true)
      ))
    }
    else if (value.possibleTypes.intersect(concreteTypes).isEmpty) {
      // Cannot be of this type
      Some(PlanResult(
        state=initialState,
        new iv.ConstantBooleanValue(false)
      ))
    }
    else {
      concreteTypes.toList match {
        case List(singleType) =>
          val datumCellTemp = value.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
          val testPredicate = ps.Temp(vt.Predicate) 

          plan.steps += ps.TestCellType(testPredicate, datumCellTemp, singleType)

          Some(PlanResult(
            state=initialState,
            new iv.NativePredicateValue(testPredicate)
          ))

        case _ =>
          // XXX: We don't support testing for multiple types yet
          None
      }
    }
  }

  def apply(initialState : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult]  
}
