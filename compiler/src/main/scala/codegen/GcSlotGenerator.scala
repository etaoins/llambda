package io.llambda.compiler.codegen
import io.llambda

import collection.mutable

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.platform.TargetPlatform
import llambda.llvmir._

class GcSlotGenerator(entryBlock : IrEntryBlockBuilder)(worldPtrIr : IrValue, nextBlock : IrChildBlockBuilder, targetPlatform : TargetPlatform) extends {
  private val blockTerminators = new mutable.ListBuffer[(IrBlockBuilder, () => Unit)]
 
  def unrootAllAndTerminate(block : IrBlockBuilder)(terminatingProc : () => Unit) {
    blockTerminators.append((block, terminatingProc))
  }

  def finish(finalGcState : GcState) {
    // Allocate all the GC slots we need
    val slotCount = finalGcState.nextUnallocatedSlot 

    if (slotCount == 0) {
      entryBlock.comment("No shadow stack entry required")
      entryBlock.uncondBranch(nextBlock)

      // Call all the block terminators
      for(terminatingProc <- blockTerminators.map(_._2)) {
        terminatingProc()
      }

      return
    }
    
    // The stack shadow stack header is {next*, i64} 
    // On 32bit we need 3 pointer widths for this, while 64bit is 2 pointers
    val headerPointerCount = 1 + (64 / targetPlatform.pointerBits)

    // Get the raw space for the shadow stack entry
    val shadowStackSpace = entryBlock.alloca("shadowStackSpace")(
      irType=PointerType(IntegerType(8)),
      numElements=headerPointerCount + slotCount
    )

    // Cast this to a shadow stack entry
    val shadowStackEntry = entryBlock.bitcastTo("shadowStackEntry")(
      value=shadowStackSpace,
      toType=PointerType(ShadowStackEntryValue.irType)
    )

    // Add ourselves to the world's shadow stack linked list
    entryBlock.comment("Adding shadow stack entry to linked list")

    val oldShadowStackHead = WorldValue.genLoadFromShadowStackHead(entryBlock)(worldPtrIr) 
    ShadowStackEntryValue.genStoreToNext(entryBlock)(oldShadowStackHead, shadowStackEntry)
    WorldValue.genStoreToShadowStackHead(entryBlock)(shadowStackEntry, worldPtrIr)

    entryBlock.comment("Storing root cell count")

    // Store our size
    ShadowStackEntryValue.genStoreToCellCount(entryBlock)(
      IntegerConstant(IntegerType(64), slotCount),
      shadowStackEntry
    )
      
    val rootsArray = ShadowStackEntryValue.genPointerToRoots(entryBlock)(shadowStackEntry)

    // Initialize all the variables
    entryBlock.comment("Initializing root cells")
    val slotVariables = (0 until slotCount).map(GcSlotGenerator.irValueForSlot)

    for((variable, index) <- slotVariables.zipWithIndex) { 
      // Calculate our slot's index
      val gepIndices = List(0, index).map(IntegerConstant(IntegerType(64), _))
      val slotPtr = entryBlock.getelementptr(variable)(PointerType(ct.AnyCell.irType), rootsArray, gepIndices, inbounds=true)
      
      // Initialize the field to null
      entryBlock.store(NullPointerConstant(GcSlotGenerator.slotValueType), slotPtr)
    }

    for((block, terminatingProc) <- blockTerminators.toList) {
      // Restore the old shadow stack head
      block.comment("Removing shadow stack entry from the linked list")
      WorldValue.genStoreToShadowStackHead(block)(oldShadowStackHead, worldPtrIr) 

      terminatingProc()
    }
      
    entryBlock.uncondBranch(nextBlock)
  }
}

object GcSlotGenerator {
  val slotValueType = PointerType(ct.AnyCell.irType)
  val slotPointerType = PointerType(slotValueType)

  def irValueForSlot(slot : Int) = {
    val slotName = s"gcSlot${slot}"

    // This will be created in the entry block so it will be availble in all child blocks
    LocalVariable(slotName, slotPointerType)
  }

}
