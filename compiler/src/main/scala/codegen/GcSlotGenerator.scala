package io.llambda.compiler.codegen
import io.llambda

import collection.mutable

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.llvmir._

class GcSlotGenerator(module : IrModuleBuilder, entryBlock : IrEntryBlockBuilder)(worldPtrIr : IrValue, nextBlock : IrChildBlockBuilder) extends {
  private val allocatedSlots = new mutable.HashMap[ps.TempValue, LocalVariable]
  private def needsShadowStackEntry = allocatedSlots.size > 0

  private val blockTerminators = new mutable.ListBuffer[(IrBlockBuilder, () => Unit)]

  def apply(tempValue : ps.TempValue) : IrValue = 
    allocatedSlots(tempValue)

  def getOrElseCreate(tempValue : ps.TempValue, irType : FirstClassType) : IrValue = {
    allocatedSlots.getOrElseUpdate(tempValue, {
      val slotName = s"gcSlot${allocatedSlots.size + 1}"

      // This will be created in the entry block so it will be availble in all child blocks
      LocalVariable(slotName, PointerType(irType))
    })
  }

  def unrootAllAndTerminate(block : IrBlockBuilder)(terminatingProc : () => Unit) {
    blockTerminators.append((block, terminatingProc))
  }

  def finish() {
    // Allocate all the GC slots we need
    val slotCount = allocatedSlots.size

    if (!needsShadowStackEntry) {
      entryBlock.comment("No shadow stack entry required")
      entryBlock.uncondBranch(nextBlock)

      // Call all the block terminators
      for(terminatingProc <- blockTerminators.map(_._2)) {
        terminatingProc()
      }

      return
    }

    // Get the raw space for the shadow stack entry
    val shadowStackSpace = entryBlock.alloca("shadowStackSpace")(
      irType=PointerType(IntegerType(8)),
      numElements=slotCount + 2
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
    val slotVariables = allocatedSlots.values.toList.sortBy(_.name) 

    for((variable, index) <- slotVariables.zipWithIndex) {
      val rawSlotType = PointerType(PointerType(ct.DatumCell.irType))
      val targetType = variable.irType
        
      // Calculate our slot's index
      val gepIndices = List(0, index).map(IntegerConstant(IntegerType(64), _))

      if (targetType != rawSlotType) {
        val uncastSlot = entryBlock.getelementptr("uncastSlot")(PointerType(ct.DatumCell.irType), rootsArray, gepIndices, inbounds=true)

        entryBlock.bitcastTo(variable)(uncastSlot, targetType)
      }
      else {
        // No bitcast needed
        entryBlock.getelementptr(variable)(PointerType(ct.DatumCell.irType), rootsArray, gepIndices, inbounds=true)
      }
      
      // Initialize the field to null
      targetType match {
        case PointerType(innerPointerType : PointerType) =>
          entryBlock.store(NullPointerConstant(innerPointerType), variable)

        case _ =>
          throw new InternalCompilerErrorException("GC slots must be pointers to cells")
      }
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
