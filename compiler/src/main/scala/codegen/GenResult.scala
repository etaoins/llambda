package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{StorageLocation, InternalCompilerErrorException}
import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

sealed abstract class GenResult 

case class GenerationState(
  module : IrModuleBuilder,
  gcSlots : GcSlotGenerator,
  currentBlock : IrBlockBuilder,
  currentAllocation : CellAllocation,
  gcCleanUpBlockOpt : Option[IrBranchTarget],
  liveTemps : Map[ps.TempValue, IrValue] = Map(),
  gcRootedTemps : Set[ps.TempValue] = Set()
) extends GenResult {
  def withTempValue(tempValue : (ps.TempValue, IrValue)) =
    this.copy(liveTemps=(liveTemps + tempValue))
}

case object BlockTerminated extends GenResult
