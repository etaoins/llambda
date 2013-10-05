package llambda.codegen

import llambda.StorageLocation
import llambda.codegen.llvmir.{IrModuleBuilder, IrBlockBuilder}

case class GenerationState(
  module : IrModuleBuilder,
  currentBlock : IrBlockBuilder,
  liveVariables : Map[StorageLocation, LiveValue] = Map())

