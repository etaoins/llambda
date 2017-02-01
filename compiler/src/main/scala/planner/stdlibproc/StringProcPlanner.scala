package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object StringProcPlanner extends StdlibProcPlanner {
  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("string-length", List((_, iv.ConstantStringValue(stringValue)))) =>
      // This is easy
      val codePoints = stringValue.codePointCount(0, stringValue.length)

      Some(iv.ConstantIntegerValue(codePoints))

    case _ =>
      None
  }
}
