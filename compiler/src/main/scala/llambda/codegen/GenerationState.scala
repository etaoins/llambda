package llambda.codegen

import llambda.StorageLocation
import llambda.codegen.llvmir.{IrModuleBuilder, IrBlockBuilder, IrValue}
import llambda.planner.{step => ps}

case class GenerationState(
  module : IrModuleBuilder,
  currentBlock : IrBlockBuilder,
  liveAllocations : Map[ps.TempAllocation, GenCellAllocation.CellAllocation] = Map(),
  liveTemps : Map[ps.TempValue, IrValue] = Map()
) {
  def withAllocation(allocation : (ps.TempAllocation, GenCellAllocation.CellAllocation)) =
    this.copy(liveAllocations=(liveAllocations + allocation))
  
  def withTempValue(tempValue : (ps.TempValue, IrValue)) =
    this.copy(liveTemps=(liveTemps + tempValue))
}
