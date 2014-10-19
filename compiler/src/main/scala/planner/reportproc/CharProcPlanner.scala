package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CharProcPlanner extends ReportProcPlanner {
  override def planWithValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("char->integer", List((_, constantChar : iv.ConstantCharValue))) =>
      Some(new iv.ConstantExactIntegerValue(constantChar.value))

    case ("char->integer", List((charLocated, charValue))) =>
      val int32Temp = plan.withContextLocation(charLocated) {
        charValue.toTempValue(vt.UnicodeChar)
      }

      Some(new iv.NativeExactIntegerValue(int32Temp, vt.Int32))

    case ("integer->char", List((_, constantInt : iv.ConstantExactIntegerValue))) =>
      Some(new iv.ConstantCharValue(constantInt.value.toInt))

    case ("integer->char", List((intLocated, intValue))) =>
      val int32Temp = plan.withContextLocation(intLocated) {
        intValue.toTempValue(vt.Int32)
      }

      Some(new iv.NativeCharValue(int32Temp))

    case _ =>
      None
  }
}
