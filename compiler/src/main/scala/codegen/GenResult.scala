package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

sealed abstract class GenResult {
  val gcState : GcState
}

case class GenerationState(
  module : IrModuleBuilder,
  gcSlotsOpt : Option[GcSlotGenerator],
  currentBlock : IrBlockBuilder,
  currentAllocation : CellAllocation,
  gcCleanUpBlockOpt : Option[IrBranchTarget],
  liveTemps : LiveTemps,
  gcState : GcState
) extends GenResult {
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

    BlockTerminated(gcState)
  }

  def withTempValue(tempTuple : (ps.TempValue, IrValue)) = {
    this.copy(
      liveTemps=liveTemps + tempTuple
    )
  }

  def withoutGcSupport = {
    this.copy(
      gcSlotsOpt=None,
      gcCleanUpBlockOpt=None
    )
  }
}

case class BlockTerminated(gcState : GcState) extends GenResult
