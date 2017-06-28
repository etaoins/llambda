package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner._

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}


object StringProcPlanner extends StdlibProcPlanner with StdlibProcPlannerHelpers {
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

    case ("string-ref", List((_, iv.ConstantStringValue(str)), (_, iv.ConstantIntegerValue(index)))) =>
      val size = str.codePointCount(0, str.length)
      assertIndexValid("(string-ref)", size, index)

      val charIndex = str.offsetByCodePoints(0, index.toInt)
      Some(iv.ConstantCharValue(str.codePointAt(charIndex)))

    case ("string-append", List()) =>
      Some(iv.ConstantStringValue(""))

    case ("string-append", List((_, stringValue))) =>
      Some(stringValue)

    case ("string-copy", List((_, stringValue))) =>
      Some(stringValue)

    case ("string-copy", List((_, stringValue), (_, iv.ConstantIntegerValue(0)))) =>
      Some(stringValue)

    case _ =>
      None
  }
}
