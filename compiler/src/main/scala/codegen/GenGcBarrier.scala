package io.llambda.compiler.codegen
import io.llambda

import collection.immutable.ListMap

import llambda.compiler.planner.{step => ps}
import llambda.compiler.InternalCompilerErrorException
import llambda.llvmir._

object GenGcBarrier {
  case class RestoreData(
    loadFromSlot: Int,
    previousIrValue: IrValue,
    restoreToTemp: ps.TempValue
  )

  case class CalculatedBarrier(
    nullSlots: List[Int],
    saveSlots: List[(IrValue, Int)],
    restoreTemps: List[RestoreData],
    finalGcState: GcState
  )

  /** Calculates a GC barrier without generating any code
    *
    * @param  flushFilterOpt  If defined this will instead calculate a GC flush only rooting the passed values. GC
    *                         flushes are used to root values that required rooting on both sides of a conditional
    *                         branch
    */
  def calculateGcBarrier(state: GenerationState)(flushFilterOpt: Option[Set[ps.TempValue]] = None): CalculatedBarrier = {
    val liveTemps = state.liveTemps
    val gcState = state.gcState
    val flushOnly = flushFilterOpt.isDefined

    if (!state.currentAllocation.isEmpty && !flushOnly) {
      // Allocations cannot cross GC barriers
      throw new InternalCompilerErrorException("Cell allocation held across GC barrier")
    }

    // We only care about GC managed values
    // Sort them by their IR representation to ensure we generate stable IR between runs
    val liveGcManagedValues = liveTemps.tempValueToIr.toSeq.filter(_._1.isGcManaged).sortBy(_._2.toIr)

    // If we have a root filter due to a GC flush we might be only rooting some GC managed values
    val flushingGcManagedValues = flushFilterOpt match {
      case Some(filterTemps) =>
        liveGcManagedValues.filter { case (tempValue, irValue) =>
          filterTemps.contains(tempValue)
        }

      case _ =>
        liveGcManagedValues
    }

    // Build a set of all pointer identities that are live, even if they aren't being flushed
    val liveIdentities = Set[GcPointerIdentity](liveGcManagedValues.map({ case (tempValue, _) =>
      liveTemps.pointerIdentities(tempValue)
    }): _*)

    // If multiple values with the same identity need to be saved we should only save one
    val flushingIdentities = ListMap(flushingGcManagedValues.map({ case (tempValue, irValue) =>
      // Find their identities and pick an arbitrary one to save
      liveTemps.pointerIdentities(tempValue) -> irValue
    }): _*)

    // Find the set of values that need to be flushed to memory
    val unflushedIdentities = flushingIdentities.filter({ case (pointerIdentity, irValue) =>
      // Filter out any identities already rooted
      !gcState.rootedIdentities.contains(pointerIdentity)
    })

    // Figure out which slots we weren't using before entering this barrier
    val idleSlots = (0 until gcState.nextUnallocatedSlot).toSet -- gcState.rootedIdentities.values.toSet

    // Find the set of values that need to be derooted
    val newlyDerootedIdentities = gcState.rootedIdentities.keySet -- liveIdentities
    val newlyDerootedSlots = newlyDerootedIdentities.map(gcState.rootedIdentities)

    // Figure out if we can steal any slots
    // Prefer the newly derooted slots because then we can skip nulling them entirely
    val availableSlots = newlyDerootedSlots.toList.sorted ++ idleSlots.toList.sorted

    // Now calculate how many slots we can steal
    val stealSlotCount = unflushedIdentities.size.min(availableSlots.size)

    // Split our flushing list in to ones that are stealing slots and ones that are allocating new slots
    val (stealingFlushes, newSlotFlushes) = unflushedIdentities.splitAt(stealSlotCount)

    // Zip the stolen roots with the slots to just steal them directly
    val stolenIdentitiesToSlot = stealingFlushes.map(_._1).zip(availableSlots).toMap: Map[GcPointerIdentity, Int]

    // Allocate slots for the unstolen roots
    val nextUnallocatedSlot = gcState.nextUnallocatedSlot + newSlotFlushes.size
    val newSlotRange = gcState.nextUnallocatedSlot to nextUnallocatedSlot

    // Assign the unstolen roots
    val unstolenIdentitiesToSlot = newSlotFlushes.map(_._1).zip(newSlotRange).toMap: Map[GcPointerIdentity, Int]

    // Build our save list in numeric slot order
    val saveSlots = ((stolenIdentitiesToSlot ++ unstolenIdentitiesToSlot) map { case (pointerIdentity, slot) =>
      unflushedIdentities(pointerIdentity) -> slot
    }).toList.sortBy(_._2)

    // Any derooted identites we didn't steal need to be zeroed before they're put in the availabe slot pool
    val nullSlots = (newlyDerootedSlots -- stolenIdentitiesToSlot.values).toList.sorted

    // Now find all of rooted identities, including previously flushed ones
    val rootedIdentities = (gcState.rootedIdentities -- newlyDerootedIdentities) ++ stolenIdentitiesToSlot ++ unstolenIdentitiesToSlot

    // Generate restore information for them in slot order
    val restoreTemps = if (!flushOnly) {
      (liveGcManagedValues.map { case (tempValue, previousIrValue) =>
        val loadFromSlot = rootedIdentities(liveTemps.pointerIdentities(tempValue))

        RestoreData(
          loadFromSlot=loadFromSlot,
          previousIrValue=previousIrValue,
          restoreToTemp=tempValue
        )
      }).toList.sortBy(_.loadFromSlot)
    }
    else {
      // We can't restore if we're flushing
      Nil
    }

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

  def genSaveGcRoots(state: GenerationState)(calcedBarrier: CalculatedBarrier, saveDescription: String = "GC barrier") {
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
  def genRestoreGcRoots(state: GenerationState)(calcedBarrier: CalculatedBarrier): List[IrValue] = {
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

  def flushGcRoots(state: GenerationState)(toFlush: Set[ps.TempValue]): GenerationState = {
    if (toFlush.isEmpty) {
      // Nothing to do!
      state
    }
    else {
      val calcedBarrier = calculateGcBarrier(state)(Some(toFlush))
      genSaveGcRoots(state)(calcedBarrier, "GC flush")

      state.copy(
        gcState=calcedBarrier.finalGcState
      )
    }
  }

  def apply[T](initialState: GenerationState)(innerBlock: => (IrBlockBuilder, T)): (GenerationState, T) = {
    val calcedBarrier = calculateGcBarrier(initialState)(None)

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
