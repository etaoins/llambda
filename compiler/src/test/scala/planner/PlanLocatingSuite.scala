package io.llambda.compiler.planner
import io.llambda

import org.scalatest.FunSuite
import scala.io.Source

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class PlanLocatingSuite extends FunSuite with PlanHelpers {
  private def assertPlanLocated(plannedFunctions : Map[String, planner.PlannedFunction], description : String) {
    for((symbol, function) <- plannedFunctions if !function.isArtificial) {
      for(step <- function.steps) {
        step match {
          case _ : ps.CastCellToTypeUnchecked =>
            // This doesn't generate any assembler code - allow this to be unlocated
            // Otherwise we'll blow up on the InferArgumentTypes' magic to keep the original untyped argument temp alive

          case _ : ps.AllocateCells =>
            // This doesn't come from a particular source line - these are inserted by PlanCellAllocations:w

          case ps.LoadRecordLikeData(_, _, _ : vt.ClosureType) |
               ps.LoadRecordDataField(_, _, _ : vt.ClosureType, _) =>
            // This is loading from a closure in a lambda prologue

          case ps.Return(None) if symbol == codegen.LlambdaTopLevelSignature.nativeSymbol =>
            // This is produced entirely by codegen

          case _ =>
            assert(step.hasLocation, s"${step} in function '${symbol}' has no source location ${description}")
        }
      }
    }
  }

  test("all non-housekeeping steps are source located after planning") {
    val programData = testutil.NonTrivialProgram.data
    
    val functions = planForData(
      data=programData,
      optimise=true,
      reduce=false,
      includePath=testutil.NonTrivialProgram.includePath
    )

    assertPlanLocated(functions, "after planning")
  }
  
  test("all non-housekeeping steps are source located after reduction and planning") {
    val programData = testutil.NonTrivialProgram.data
    
    val functions = planForData(
      data=programData,
      optimise=true,
      reduce=true,
      includePath=testutil.NonTrivialProgram.includePath
    )

    assertPlanLocated(functions, "after reduction and planning")
  }
}
