package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}


sealed abstract class GenResult

case class GenerationState(
  currentBlock: IrBlockBuilder,
  currentAllocation: HeapAllocation,
  liveTemps: Map[ps.TempValue, IrValue]
) extends GenResult {
  def withTempValue(tempTuple: (ps.TempValue, IrValue)) = {
    this.copy(liveTemps=liveTemps + tempTuple)
  }

  def withDisposedValues(disposedValues: Set[ps.TempValue]) = {
    this.copy(liveTemps=liveTemps -- disposedValues)
  }
}

case object BlockTerminated extends GenResult
