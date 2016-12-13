package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

sealed abstract class GenResult {
  val gcState: GcState
}

case class GenerationState(
  gcSlotsOpt: Option[GcSlotGenerator],
  currentBlock: IrBlockBuilder,
  currentAllocation: CellAllocation,
  gcCleanUpBlockOpt: Option[IrBranchTarget],
  liveTemps: LiveTemps,
  gcState: GcState
) extends GenResult {
  def terminateFunction(terminatorProc: () => Unit): BlockTerminated = {
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

  def withTempValue(tempTuple: (ps.TempValue, IrValue)) = {
    this.copy(
      liveTemps=liveTemps + tempTuple
    )
  }

  def withDisposedValues(disposedValues: Set[ps.TempValue]) = {
    this.copy(liveTemps=liveTemps -- disposedValues)
  }
}

case class BlockTerminated(gcState: GcState) extends GenResult
