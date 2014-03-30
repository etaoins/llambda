package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.platform.TargetPlatform

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
      linkage=Linkage.Internal,
      gc=Some("shadow-stack")
    ) 

    // Create a blank generation state with just our args
    val argTemps = (plannedFunction.namedArguments map { case (name, tempValue) =>
      (tempValue, generatedFunction.argumentValues(name))
    }).toMap
    
    // Create a GC slot generator and have it own the entry block
    val gcSlots = new GcSlotGenerator(module, generatedFunction.entryBlock)

    val procStartBlock = generatedFunction.entryBlock.startChildBlock("procStart")

    // Create our landingpad if we need one
    val gcCleanUpBlockOpt = plannedFunction.worldPtrOption map { _ =>
      val block = generatedFunction.entryBlock.startChildBlock("gcCleanUp")
      GenGcCleanUpBlock(module, block)
      block
    }

    val startState = GenerationState(
      module=module,
      gcSlots=gcSlots,
      currentBlock=procStartBlock,
      currentAllocation=EmptyCellAllocation(),
      liveTemps=argTemps,
      gcCleanUpBlockOpt=gcCleanUpBlockOpt
    )

    // Generate our steps
    GenPlanSteps(startState, plannedSymbols, typeGenerator)(plannedFunction.steps)

    // Finalize the GC slot generator so it jumps to the proc start
    gcSlots.finalize(procStartBlock)
      
    // Define the function
    module.defineFunction(generatedFunction)
  }
}
