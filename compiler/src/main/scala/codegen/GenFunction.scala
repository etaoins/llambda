package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.platform.TargetPlatform
import llambda.compiler.planner.{step => ps}

private[codegen] object GenFunction {
  def apply(module : IrModuleBuilder, genGlobals : GenGlobals)(nativeSymbol : String, plannedFunction : planner.PlannedFunction) {
    val irSignature = ProcedureSignatureToIr(plannedFunction.signature)

    val argumentNames = plannedFunction.namedArguments.map(_._1)
    val namedIrArguments = argumentNames.zip(irSignature.arguments)

    // This function does not need to be externally accessible
    // This allows LLVM to more aggressively optimize and reduces the chance of symbol conflicts with other objects
    val generatedFunction = new IrFunctionBuilder(
      module=module,
      result=irSignature.result,
      namedArguments=namedIrArguments,
      name=nativeSymbol,
      linkage=Linkage.Internal
    ) 

    // Create a blank generation state with just our args
    val argTemps = plannedFunction.namedArguments.foldLeft(LiveTemps()) { case (liveTemps, (name, tempValue)) =>
      liveTemps + (tempValue -> generatedFunction.argumentValues(name))
    }

    // Do we need to set up GC for this function?
    val (procStartBlock, gcSlotsOpt, gcCleanUpBlockOpt) = plannedFunction.worldPtrOpt match {
      case Some(worldPtrTemp) =>
        val worldPtrIr = argTemps(worldPtrTemp)

        // Create the start block the GC code invokes after the entry block
        val procStartBlock = generatedFunction.startChildBlock("procStart")
        
        // Create our GC slot allocator
        val gcSlots = new GcSlotGenerator(generatedFunction.entryBlock)(worldPtrIr, procStartBlock, genGlobals.targetPlatform)

        // Create our landingpad
        val gcCleanUpBlock = {
          val block = generatedFunction.startChildBlock("gcCleanUp")
          GenGcCleanUpBlock(block, gcSlots)
          block
        }

        (procStartBlock, Some(gcSlots), Some(gcCleanUpBlock))

      case None =>
        // No GC support
        (generatedFunction.entryBlock, None, None)
    }

    val startState = GenerationState(
      gcSlotsOpt=gcSlotsOpt,
      currentBlock=procStartBlock,
      currentAllocation=EmptyCellAllocation(),
      gcCleanUpBlockOpt=gcCleanUpBlockOpt,
      liveTemps=argTemps,
      gcState=GcState()
    )

    // Generate our steps
    val finalResult = GenPlanSteps(startState, genGlobals)(plannedFunction.steps)

    for(gcSlots <- gcSlotsOpt) {
      gcSlots.finish(finalResult.gcState)
    }
      
    // Define the function
    module.defineFunction(generatedFunction)
  }
}
