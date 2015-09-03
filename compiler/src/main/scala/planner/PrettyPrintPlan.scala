package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object PrettyPrintPlan {
  private def stepToString(step : ps.Step) : String = step match {
    case ps.CondBranch(test, trueSteps, falseSteps, valuesPhi) =>
      val resultValues = valuesPhi.map(_.result).mkString(" ")
      val trueValues = valuesPhi.map(_.trueValue).mkString(" ")
      val falseValues = valuesPhi.map(_.falseValue).mkString(" ")

      s"CondBranch(${resultValues}, ${test}) {\n" +
        trueSteps.map(stepToString).flatMap(_.split("\n")).map("  " + _).mkString("\n") + "\n" +
      s"} => ${trueValues}\n" +
      "else {\n" +
        falseSteps.map(stepToString).flatMap(_.split("\n")).map("  " + _).mkString("\n") + "\n" +
      s"} => ${falseValues}"

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

