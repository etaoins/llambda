package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}


sealed abstract class GenResult

case class GenerationState(
  currentBlock: IrBlockBuilder,
  currentAllocation: HeapAllocation,
  liveTemps: LiveTemps
) extends GenResult {
  def withTempValue(tempTuple: (ps.TempValue, IrValue), gcRoot: Boolean) = {
    val civ = CollectableIrValue(tempTuple._2, gcRoot)

    this.copy(
      liveTemps=liveTemps + (tempTuple._1 -> civ)
    )
  }

  def withDisposedValues(disposedValues: Set[ps.TempValue]) = {
    this.copy(liveTemps=liveTemps -- disposedValues)
  }
}

case object BlockTerminated extends GenResult
