package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

sealed abstract class GenResult 

case class GenerationState(
  module : IrModuleBuilder,
  gcSlotsOpt : Option[GcSlotGenerator],
  currentBlock : IrBlockBuilder,
  currentAllocation : CellAllocation,
  gcCleanUpBlockOpt : Option[IrBranchTarget],
  liveTemps : Map[ps.TempValue, IrValue] = Map(),
  gcRootedTemps : Set[ps.TempValue] = Set()
) extends GenResult {
  def withTempValue(tempValue : (ps.TempValue, IrValue)) =
    this.copy(liveTemps=(liveTemps + tempValue))

  def terminateFunction(terminatorProc : () => Unit) = {
    gcSlotsOpt match {
      case Some(gcSlots) =>
        // Make sure to clean up our GC state
        // terminatorProc will be called by our GC code at the end of codegen for this function
        gcSlots.unrootAllAndTerminate(currentBlock)(terminatorProc)

      case None =>
        // Just terminate
        terminatorProc()
    }

    BlockTerminated
  }
}

case object BlockTerminated extends GenResult
