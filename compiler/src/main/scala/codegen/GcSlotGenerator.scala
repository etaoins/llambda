package io.llambda.compiler.codegen
import io.llambda

import collection.mutable

import llambda.compiler.{celltype => ct}
import llambda.compiler.platform.TargetPlatform
import llambda.llvmir._

class GcSlotGenerator(entryBlock: IrEntryBlockBuilder)(worldPtrIr: IrValue, nextBlock: IrChildBlockBuilder, targetPlatform: TargetPlatform) extends {
  private val blockTerminators = new mutable.ListBuffer[(IrBlockBuilder, () => Unit)]
  private val module = entryBlock.function.module

  def unrootAllAndTerminate(block: IrBlockBuilder)(terminatingProc: () => Unit) {
    blockTerminators.append((block, terminatingProc))
  }

  def finish(finalGcState: GcState) {
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

    // Create the type with the exact number of slots we need so we will size and align it correctly
    val shadowStackTypeName = s"shadowStackEntry${slotCount}Roots"
    val shadowStackType = UserDefinedType(shadowStackTypeName)
    val rootsArrayType = ArrayType(slotCount, PointerType(ct.AnyCell.irType))

    module.unlessDeclared(shadowStackTypeName) {
      val shadowStackTypeDefinition = StructureType(List(
        ShadowStackEntryHeaderValue.irType,
        rootsArrayType
      ))

      module.nameType(shadowStackTypeName, shadowStackTypeDefinition)
    }

    // Allocate the the shadow stack entry
    val shadowStackEntry = entryBlock.alloca("shadowStackEntry")(
      irType=shadowStackType,
      numElements=IntegerConstant(IntegerType(32), 1)
    )

    // Get a pointer to the shadow stack header
    val shadowStackEntryHeader = entryBlock.getelementptr("shadowStackEntryHeader")(
      elementType=ShadowStackEntryHeaderValue.irType,
      basePointer=shadowStackEntry,
      indices=List(0, 0).map(IntegerConstant(IntegerType(32), _)),
      inbounds=true
    )

    // Add ourselves to the world's shadow stack linked list
    entryBlock.comment("Adding shadow stack entry to linked list")

    val oldShadowStackHead = WorldValue.genLoadFromShadowStackHead(entryBlock)(worldPtrIr)
    ShadowStackEntryHeaderValue.genStoreToNext(entryBlock)(oldShadowStackHead, shadowStackEntryHeader)
    WorldValue.genStoreToShadowStackHead(entryBlock)(shadowStackEntryHeader, worldPtrIr)

    entryBlock.comment("Storing root cell count")

    // Store our size
    ShadowStackEntryHeaderValue.genStoreToCellCount(entryBlock)(
      IntegerConstant(IntegerType(32), slotCount),
      shadowStackEntryHeader
    )

    val rootsArray = entryBlock.getelementptr("rootPtr")(
      elementType=rootsArrayType,
      basePointer=shadowStackEntry,
      indices=List(0, 1).map(IntegerConstant(IntegerType(32), _)),
      inbounds=true
    )

    // Initialize all the variables
    entryBlock.comment("Initializing root cells")
    val slotVariables = (0 until slotCount).map(GcSlotGenerator.irValueForSlot)

    for((variable, index) <- slotVariables.zipWithIndex) {
      // Calculate our slot's index
      val gepIndices = List(0, index).map(IntegerConstant(IntegerType(32), _))
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

  def irValueForSlot(slot: Int) = {
    val slotName = s"gcSlot${slot}"

    // This will be created in the entry block so it will be availble in all child blocks
    LocalVariable(slotName, slotPointerType)
  }

}
