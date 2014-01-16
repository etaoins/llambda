package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.StorageLocation
import llambda.llvmir.{IrModuleBuilder, IrBlockBuilder, IrValue}
import llambda.compiler.planner.{step => ps}

case class GenerationState(
  module : IrModuleBuilder,
  gcSlots : Map[ps.TempValue, IrValue],
  currentBlock : IrBlockBuilder,
  liveAllocations : Map[ps.TempAllocation, GenCellAllocation.CellAllocation] = Map(),
  liveTemps : Map[ps.TempValue, IrValue] = Map(),
  gcRootedTemps : Set[ps.TempValue] = Set()
) {
  def withAllocation(allocation : (ps.TempAllocation, GenCellAllocation.CellAllocation)) =
    this.copy(liveAllocations=(liveAllocations + allocation))
  
  def withTempValue(tempValue : (ps.TempValue, IrValue)) =
    this.copy(liveTemps=(liveTemps + tempValue))
}
