package llambda.codegen

import llambda.StorageLocation
import llambda.codegen.llvmir.{IrModuleBuilder, IrBlockBuilder, IrValue}

case class GenerationState(
  module : IrModuleBuilder,
  currentBlock : IrBlockBuilder,
  mutableVariables : Set[StorageLocation],
  liveImmutables : Map[StorageLocation, LiveValue] = Map(),
  liveMutables : Map[StorageLocation, IrValue] = Map()
)
