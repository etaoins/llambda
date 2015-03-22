package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object BooleanProcPlanner extends ReportProcPlanner {
  override def planWithValue(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("boolean=?", args) if args.length > 1 =>
      val constBoolArgs = args collect {
        case (located, constBool : iv.ConstantBooleanValue) =>
          constBool.value
      }

      if (constBoolArgs.length == args.length) {
        val result = constBoolArgs.forall(_ == constBoolArgs(0))
        Some(iv.ConstantBooleanValue(result))
      }
      else {
        None
      }

    case _ =>
      None
  }
}
