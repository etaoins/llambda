package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenGcBarrier {
  def apply[T](initialState : GenerationState)(innerBlock : => T) : (GenerationState, T) = {
    val block = initialState.currentBlock

    // All GC managed values from the initial state need to be restored
    val toRestore = initialState.liveTemps.filter(_._1.isGcManaged)
    
    if (toRestore.isEmpty) {
      return (initialState, innerBlock)
    }

    // Values that haven't already been rooted need to be saved
    val toSave = toRestore.filter { case (tempValue, _) =>
      !initialState.gcRootedTemps.contains(tempValue)
    }

    // Save all values
    block.comment("Saving GC roots before GC barrier")
    for((tempValue, irValue) <- toSave) {
      // Cast the value to a generic pointer
      val castIrValue = block.bitcastTo("gcRootCast")(irValue, PointerType(ct.DatumCell.irType))
      block.store(castIrValue, initialState.gcSlots(tempValue))
    }

    // Call the inner block
    val innerBlockResult = innerBlock

    // Restore all values
    block.comment("Restoring GC roots after GC barrier")
    
    val liveTempsUpdate = toRestore.map { case (tempValue, prevIrValue) =>
      val newIrValue = block.load("restoredGcRoot")(initialState.gcSlots(tempValue))

      // Cast to the original value
      val castIrValue = block.bitcastTo("gcRootCast")(newIrValue, prevIrValue.irType)

      // Update the IR value for this temp value
      (tempValue -> castIrValue)
    }

    val finalState = initialState.copy(
      liveTemps=initialState.liveTemps ++ liveTempsUpdate,
      gcRootedTemps=toRestore.keySet
    )
    
    (finalState, innerBlockResult)
  }
}
