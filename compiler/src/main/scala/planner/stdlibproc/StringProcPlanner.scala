package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner._

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}


object StringProcPlanner extends StdlibProcPlanner {
  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("string-length", List((_, iv.ConstantStringValue(str)))) =>
      // This is easy
      val codePoints = str.codePointCount(0, str.length)

      Some(iv.ConstantIntegerValue(codePoints))

    case ("string-length", List((_, stringValue))) =>
      val resultTemp = ps.TempValue()
      val stringTemp = stringValue.toTempValue(vt.StringType)

      plan.steps += ps.LoadStringCharLength(resultTemp, stringTemp)

      Some(TempValueToIntermediate(vt.UInt32, resultTemp))

    case _ =>
      None
  }
}
