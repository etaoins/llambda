package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}

object FindUsedFunctions {
  private def usedFunctionsForSteps(
      plannedFunctions: Map[String, PlannedFunction],
      steps: List[ps.Step],
      initialUsedFunctions: Map[String, PlannedFunction]
  ): Map[String, PlannedFunction] =
    steps.foldLeft(initialUsedFunctions) { case (usedFunctions, step) =>
      step match {
        case ps.CreateNamedEntryPoint(_, _, nativeSymbol) =>
          if (!usedFunctions.contains(nativeSymbol)) {
            FindUsedFunctions(plannedFunctions, nativeSymbol, usedFunctions)
          }
          else {
            usedFunctions
          }

        case nestingStep: ps.NestingStep =>
          val allInnerSteps = nestingStep.innerBranches.flatMap(_._1)
          usedFunctionsForSteps(plannedFunctions, allInnerSteps, usedFunctions)

        case _ =>
          usedFunctions
      }
    }

  /** Finds all planned native symbols used by a given entry symbol
    *
    * @param  plannedFunctions  Map of all planned functions
    * @param  entrySymbol       Native symbol to enter. If the Llambda top-level function is used this will find all
    *                           used functions in the program
    * @param  usedFunctions     Map of functions already known to be used. They will be returned and not re-entered
    * @return  Map of used planned functions. Note that unplanned native functions (such as C++ library functions) will
    *          not be returned
    */
  def apply(
      plannedFunctions: Map[String, PlannedFunction],
      entrySymbol: String,
      usedFunctions: Map[String, PlannedFunction] = Map()
  ): Map[String, PlannedFunction] = {
    plannedFunctions.get(entrySymbol) match {
      case Some(plannedFunction) =>
        val newUsedFunctions = usedFunctions + (entrySymbol -> plannedFunction)
        usedFunctionsForSteps(plannedFunctions, plannedFunction.steps, newUsedFunctions)

      case None =>
        // This is an externally defined function
        usedFunctions
    }
  }
}
