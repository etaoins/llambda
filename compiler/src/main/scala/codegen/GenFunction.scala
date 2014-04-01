package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}

private[codegen] object GenFunction {
  def apply(module : IrModuleBuilder, plannedSymbols : Set[String], typeGenerator : TypeGenerator)(nativeSymbol : String, plannedFunction : planner.PlannedFunction) {
    val irSignature = ProcedureSignatureToIr(plannedFunction.signature)

    val argumentNames = plannedFunction.namedArguments.map(_._1)
    val namedIrArguments = argumentNames.zip(irSignature.arguments)

    // This function does not need to be externally accessible
    // This allows LLVM to more aggressively optimize and reduces the chance
    // of symbol conflicts with other objects
    val generatedFunction = new IrFunctionBuilder(
      result=irSignature.result,
      namedArguments=namedIrArguments,
      name=nativeSymbol,
      linkage=Linkage.Internal
    ) 

    // Create a blank generation state with just our args
    val argTemps = (plannedFunction.namedArguments map { case (name, tempValue) =>
      (tempValue, generatedFunction.argumentValues(name))
    }).toMap
    
    // Do we need to set up GC for this function?
    val (procStartBlock, gcSlotsOpt, gcCleanUpBlockOpt) = plannedFunction.worldPtrOption match {
      case Some(worldPtrTemp) =>
        val worldPtrIr = argTemps(worldPtrTemp)

        // Create the start block the GC code invokes after the entry block
        val procStartBlock = generatedFunction.entryBlock.startChildBlock("procStart")
        
        // Create our GC slot allocator
        val gcSlots = new GcSlotGenerator(module, generatedFunction.entryBlock)(worldPtrIr, procStartBlock)

        // Create our landingpad
        val gcCleanUpBlock = {
          val block = generatedFunction.entryBlock.startChildBlock("gcCleanUp")
          GenGcCleanUpBlock(module, block, gcSlots)
          block
        }

        (procStartBlock, Some(gcSlots), Some(gcCleanUpBlock))

      case None =>
        // No GC support
        (generatedFunction.entryBlock, None, None)
    }

    val startState = GenerationState(
      module=module,
      gcSlotsOpt=gcSlotsOpt,
      currentBlock=procStartBlock,
      currentAllocation=EmptyCellAllocation(),
      liveTemps=argTemps,
      gcCleanUpBlockOpt=gcCleanUpBlockOpt
    )

    // Generate our steps
    GenPlanSteps(startState, plannedSymbols, typeGenerator)(plannedFunction.steps)

    for(gcSlots <- gcSlotsOpt) {
      gcSlots.finish()
    }
      
    // Define the function
    module.defineFunction(generatedFunction)
  }
}
