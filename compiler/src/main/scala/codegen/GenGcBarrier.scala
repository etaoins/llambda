package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenGcBarrier {
  case class CalculatedBarrier(
    saveTemps : List[(ps.TempValue, IrValue)],
    restoreTemps : List[(ps.TempValue, IrValue)],
    finalGcRootedTemps : Set[ps.TempValue]
  )

  def calculateGcBarrier(state: GenerationState) : CalculatedBarrier = {
    // All GC managed values from the initial state need to be restored
    val toRestoreValues = state.liveTemps.filter(_._1.isGcManaged)
    
    // Values that haven't already been rooted need to be saved
    val toSaveValues = (toRestoreValues.filter { case (tempValue, _) =>
      !state.gcRootedTemps.contains(tempValue)
    })

    CalculatedBarrier(
      saveTemps=toSaveValues.toList,
      restoreTemps=toRestoreValues.toList,
      finalGcRootedTemps=toRestoreValues.keySet
    )
  }

  def genSaveGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier) {
    val block = state.currentBlock

    // Save all values
    if (!calcedBarrier.saveTemps.isEmpty) {
      block.comment("Saving GC roots before GC barrier")
    }

    for((tempValue, irValue) <- calcedBarrier.saveTemps) {
      // Cast the value to a generic pointer
      val castIrValue = block.bitcastTo("gcRootCast")(irValue, PointerType(ct.DatumCell.irType))
      block.store(castIrValue, state.gcSlots(tempValue))
    }
  }

  // Returns a list of new IR values in the same order as calcedBarrier.restoreTemps
  def genRestoreGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier) : List[IrValue] = {
    val block = state.currentBlock

    if (!calcedBarrier.restoreTemps.isEmpty) {
      block.comment("Restoring GC roots after GC barrier")
    }
    
    (calcedBarrier.restoreTemps.map { case (tempValue, prevIrValue) =>
      val newIrValue = block.load("restoredGcRoot")(state.gcSlots(tempValue))

      // Cast to the original value
      val castIrValue = block.bitcastTo("gcRootCast")(newIrValue, prevIrValue.irType)

      // Return the new value for this temp
      castIrValue
    })
  }

  def apply[T](initialState : GenerationState)(innerBlock : => T) : (GenerationState, T) = {
    val calcedBarrier = calculateGcBarrier(initialState)
    val block = initialState.currentBlock
    
    genSaveGcRoots(initialState)(calcedBarrier)

    // Call the inner block
    val innerBlockResult = innerBlock

    // Restore all values
    val newTempValues = genRestoreGcRoots(initialState)(calcedBarrier)

    val liveTempsUpdate = calcedBarrier.restoreTemps.map(_._1).zip(newTempValues)

    val finalState = initialState.copy(
      liveTemps=initialState.liveTemps ++ liveTempsUpdate,
      gcRootedTemps=calcedBarrier.finalGcRootedTemps
    )
    
    (finalState, innerBlockResult)
  }
}
