package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object PrettyPrintPlan {
  private def stepToString(step : ps.Step) : String = step match {
    case ps.CondBranch(result, test, trueSteps, trueValue, falseSteps, falseValue) =>
      s"CondBranch(${result}, ${test}) {\n" +
        trueSteps.map(stepToString).flatMap(_.split("\n")).map("  " + _).mkString("\n") + "\n" +
      s"} => ${trueValue}\n" +
      "else {\n" +
        falseSteps.map(stepToString).flatMap(_.split("\n")).map("  " + _).mkString("\n") + "\n" +
      s"} => ${falseValue}"

    case other : ps.Step =>
      other.toString
  }

  def apply(plannedFunctions : Map[String, PlannedFunction]) : String = {
    plannedFunctions.map({ case (nativeSymbol, plannedFunction) =>
      nativeSymbol + ":\n" +
      plannedFunction.steps.map(stepToString).mkString("\n")
    }).mkString("\n\n")
  }
}

