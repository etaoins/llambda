package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException
import llambda.llvmir._

object GenGcBarrier {
  case class RestoreData(
    loadFromSlot : Int,
    previousIrValue : IrValue,
    restoreToTemp : ps.TempValue
  )

  case class CalculatedBarrier(
    nullSlots : Set[Int],
    saveSlots : Map[IrValue, Int],
    restoreTemps : List[RestoreData],
    finalGcState : GcState
  )

  def calculateGcBarrier(state: GenerationState) : CalculatedBarrier = {
    val liveTemps = state.liveTemps
    val gcState = state.gcState

    // We only care about GC managed values
    val liveGcManagedValues = liveTemps.tempValueToIr.filter(_._1.isGcManaged)

    // If multiple values with the same identity need to be saved we should only save one
    val liveIdentities = liveGcManagedValues.map({ case (tempValue, irValue) =>
      // Find their identities and pick an arbitrary one to save
      liveTemps.pointerIdentities(tempValue) -> irValue
    }).toMap
      
    // Find the set of values that need to be flushed to memory
    val unflushedIdentities = liveIdentities.filter({ case (pointerIdentity, irValue) => 
      // Filter out any identities already rooted
      !gcState.rootedIdentities.contains(pointerIdentity)
    })

    // Figure out which slots we weren't using before entering this barrier
    val idleSlots = (0 until gcState.nextUnallocatedSlot).toSet -- gcState.rootedIdentities.values.toSet
    
    // Find the set of values that need to be derooted
    val newlyDerootedIdentities = gcState.rootedIdentities.keySet -- liveIdentities.keySet
    val newlyDerootedSlots = newlyDerootedIdentities.map(gcState.rootedIdentities)

    // Figure out if we can steal any slots
    // Prefer the newly derooted slots because then we can skip nulling them entirely
    val availableSlots = newlyDerootedSlots.toList ++ idleSlots.toList

    // Now calculate how many slots we can steal
    val stealSlotCount = unflushedIdentities.size.min(availableSlots.size)

    // Split our flushing list in to ones that are stealing slots and ones that are allocating new slots
    val (stealingFlushes, newSlotFlushes) = unflushedIdentities.splitAt(stealSlotCount)

    // Zip the stolen roots with the slots to just steal them directly
    val stolenIdentitiesToSlot = stealingFlushes.map(_._1).zip(availableSlots).toMap : Map[GcPointerIdentity, Int]

    // Allocate slots for the unstolen roots
    val nextUnallocatedSlot = gcState.nextUnallocatedSlot + newSlotFlushes.size
    val newSlotRange = gcState.nextUnallocatedSlot to nextUnallocatedSlot

    // Assign the unstolen roots 
    val unstolenIdentitiesToSlot = newSlotFlushes.map(_._1).zip(newSlotRange).toMap : Map[GcPointerIdentity, Int]

    // Build our save list
    val saveSlots = (stolenIdentitiesToSlot ++ unstolenIdentitiesToSlot) map { case (pointerIdentity, slot) =>
      unflushedIdentities(pointerIdentity) -> slot
    }

    // Any derooted identites we didn't steal need to be zeroed before they're put in the availabe slot pool
    val nullSlots = newlyDerootedSlots -- stolenIdentitiesToSlot.values

    // Now find all of rooted identities, including previously flushed ones 
    val rootedIdentities = (gcState.rootedIdentities -- newlyDerootedIdentities) ++ stolenIdentitiesToSlot ++ unstolenIdentitiesToSlot

    val restoreTemps = (liveGcManagedValues.map { case (tempValue, previousIrValue) =>
      val loadFromSlot = rootedIdentities(liveTemps.pointerIdentities(tempValue))

      RestoreData(
        loadFromSlot=loadFromSlot,
        previousIrValue=previousIrValue,
        restoreToTemp=tempValue
      )
    }).toList
    
    CalculatedBarrier(
      nullSlots=nullSlots,
      saveSlots=saveSlots,
      restoreTemps=restoreTemps,
      finalGcState=GcState(
        rootedIdentities=rootedIdentities,
        nextUnallocatedSlot=nextUnallocatedSlot
      )
    )
  }

  def genSaveGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier, saveDescription : String = "GC barrier") {
    val block = state.currentBlock
    val slotValueType = GcSlotGenerator.slotValueType

    if (!calcedBarrier.nullSlots.isEmpty) {
      block.comment(s"Nulling dead values before ${saveDescription}")
    }

    for(slot <- calcedBarrier.nullSlots) {
      val slotPtr = GcSlotGenerator.irValueForSlot(slot)
      block.store(NullPointerConstant(slotValueType), slotPtr)
    }

    // Save all values
    if (!calcedBarrier.saveSlots.isEmpty) {
      block.comment(s"Saving live GC roots before ${saveDescription}")
    }

    for((irValue, slot) <- calcedBarrier.saveSlots) {
      val storingValue = if (irValue.irType == slotValueType) {
        // We can use this directly
        irValue
      }
      else {
        // We need to bitcast before storing
        block.bitcastTo("savingGcRootCast")(irValue, slotValueType)
      }

      val slotPtr = GcSlotGenerator.irValueForSlot(slot)
      block.store(storingValue, slotPtr)
    }
  }

  // Returns a list of new IR values in the same order as calcedBarrier.restoreTemps
  def genRestoreGcRoots(state : GenerationState)(calcedBarrier : CalculatedBarrier) : List[IrValue] = {
    val block = state.currentBlock

    if (!calcedBarrier.restoreTemps.isEmpty) {
      block.comment("Restoring GC roots after GC barrier")
    }
    
    (calcedBarrier.restoreTemps.map { case RestoreData(loadFromSlot, previousIrValue, restoreToTemp) =>
      val slotPtr = GcSlotGenerator.irValueForSlot(loadFromSlot)
      val rawValue = block.load("restoredGcRoot")(slotPtr)

      if (previousIrValue.irType == GcSlotGenerator.slotValueType) {
        // We can use this directly
        rawValue
      }
      else {
        // We need to bitcast after restoringg
        block.bitcastTo("restoredGcRootCast")(rawValue, previousIrValue.irType)
      }
    })
  }

  def flushGcRoots(state : GenerationState) : GenerationState = {
    val calcedBarrier = calculateGcBarrier(state)
    genSaveGcRoots(state)(calcedBarrier, "GC flush")

    state.copy(
      gcState=calcedBarrier.finalGcState
    )
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
    val newIrValues = genRestoreGcRoots(postInnerBlockState)(calcedBarrier)
    val tempValueToIrUpdate = calcedBarrier.restoreTemps.map(_.restoreToTemp).zip(newIrValues)

    // Open code this instead of using ++ which will assign all new pointer identities
    val finalLiveTemps = initialState.liveTemps.withUpdatedIrValues(tempValueToIrUpdate)

    val finalState = postInnerBlockState.copy(
      liveTemps=finalLiveTemps,
      gcState=calcedBarrier.finalGcState
    )
    
    (finalState, innerBlockResult)
  }
}
