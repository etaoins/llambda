package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException
import llambda.llvmir._

object GenGcBarrier {
  case class CalculatedBarrier(
    unrootTemps : Set[ps.TempValue],
    saveTemps : List[(ps.TempValue, IrValue)],
    restoreTemps : List[(ps.TempValue, IrValue)],
    finalGcRootedTemps : Set[ps.TempValue]
  )

  def calculateGcBarrier(state: GenerationState) : CalculatedBarrier = {
    // All GC managed values from the initial state need to be restored
    val liveGcManagedValues = state.liveTemps.filter(_._1.isGcManaged)
    
    // Values that haven't already been rooted need to be saved
    val toSaveValues = (liveGcManagedValues.filter { case (tempValue, _) =>
      !state.gcRootedTemps.contains(tempValue)
    })
    
    // Any values that we were rooted but are not longer liver should be unrooted
    val unrootTemps = state.gcRootedTemps -- liveGcManagedValues.keySet

    CalculatedBarrier(
      unrootTemps=unrootTemps,
      saveTemps=toSaveValues.toList,
      restoreTemps=liveGcManagedValues.toList,
      finalGcRootedTemps=liveGcManagedValues.keySet
    )
  }

  def genSaveGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier) {
    val block = state.currentBlock

    if (!calcedBarrier.unrootTemps.isEmpty) {
      block.comment("Unrooting dead values before GC barrier")
    }

    for(tempValue <- calcedBarrier.unrootTemps) {
      val gcSlot = state.gcSlots(tempValue)

      gcSlot.irType match {
        case PointerType(innerPointer : PointerType) =>
          block.store(NullPointerConstant(innerPointer), gcSlot)

        case _ =>
          // Someone rooted something they weren't supposed to
          // All cell types are handled as pointer references
          throw new InternalCompilerErrorException("Attempted to unroot non-pointer value")
      }
    }

    // Save all values
    if (!calcedBarrier.saveTemps.isEmpty) {
      block.comment("Saving live GC roots before GC barrier")
    }

    for((tempValue, irValue) <- calcedBarrier.saveTemps) {
      val gcSlot = state.gcSlots.getOrElseCreate(tempValue, irValue.irType)
      block.store(irValue, gcSlot)
    }
  }

  // Returns a list of new IR values in the same order as calcedBarrier.restoreTemps
  def genRestoreGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier) : List[IrValue] = {
    val block = state.currentBlock

    if (!calcedBarrier.restoreTemps.isEmpty) {
      block.comment("Restoring GC roots after GC barrier")
    }
    
    (calcedBarrier.restoreTemps.map { case (tempValue, prevIrValue) =>
      val gcSlot = state.gcSlots(tempValue)
      block.load("restoredGcRoot")(gcSlot)
    })
  }

  def apply[T](initialState : GenerationState)(innerBlock : => (IrBlockBuilder, T)) : (GenerationState, T) = {
    val calcedBarrier = calculateGcBarrier(initialState)
    
    genSaveGcRoots(initialState)(calcedBarrier)

    // Call the inner block
    val (successBlock, innerBlockResult) = innerBlock

    val postInnerBlockState = initialState.copy(
      currentBlock=successBlock
    )

    // Restore all values
    val newTempValues = genRestoreGcRoots(postInnerBlockState)(calcedBarrier)

    val liveTempsUpdate = calcedBarrier.restoreTemps.map(_._1).zip(newTempValues)

    val finalState = postInnerBlockState.copy(
      liveTemps=initialState.liveTemps ++ liveTempsUpdate,
      gcRootedTemps=calcedBarrier.finalGcRootedTemps
    )
    
    (finalState, innerBlockResult)
  }
}
