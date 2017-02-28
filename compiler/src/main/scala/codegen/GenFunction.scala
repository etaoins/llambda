package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.planner.{step => ps}

private[codegen] object GenFunction {
  import Implicits._

  // This is effectively the only exception personality on mainstream Unix-likes
  private val gccPersonalityV0 = GlobalVariable(
    name="__gcc_personality_v0",
    irType=PointerType(FunctionType(
      returnType=IntegerType(32),
      parameterTypes=Nil,
      hasVararg=true
    ))
  )

  private def argumentsAreGcRoots(signature: ProcedureSignature): List[Boolean] =
    if (signature.hasWorldArg) {
      false :: argumentsAreGcRoots(signature.copy(hasWorldArg=false))
    }
    else if (signature.hasSelfArg) {
      true :: argumentsAreGcRoots(signature.copy(hasSelfArg=false))
    }
    else if (signature.mandatoryArgTypes.length > 0) {
      signature.mandatoryArgTypes.map(_.isGcManaged) ++ argumentsAreGcRoots(signature.copy(mandatoryArgTypes=Nil))
    }
    else if ((signature.optionalArgTypes.length > 0) || signature.restArgMemberTypeOpt.isDefined) {
      true :: argumentsAreGcRoots(signature.copy(optionalArgTypes=Nil, restArgMemberTypeOpt=None))
    }
    else {
      Nil
    }

  def apply(module: IrModuleBuilder, genGlobals: GenGlobals)(nativeSymbol: String, plannedFunction: planner.PlannedFunction) {
    val irSignature = ProcedureSignatureToIr(plannedFunction.signature).irSignature

    val attributes = if (plannedFunction.signature.hasWorldArg) {
      Set[IrFunction.FunctionAttribute](IrFunction.PersonalityFunction(gccPersonalityV0))
    }
    else {
      Set[IrFunction.FunctionAttribute](IrFunction.NoUnwind)
    }

    val argumentNames = plannedFunction.namedArguments.map(_._1)
    val namedIrArguments = argumentNames.zip(irSignature.arguments)

    // This function does not need to be externally accessible
    // This allows LLVM to more aggressively optimise and reduces the chance of symbol conflicts with other objects
    val generatedFunction = new IrFunctionBuilder(
      module=module,
      result=irSignature.result,
      namedArguments=namedIrArguments,
      name=nativeSymbol,
      attributes=attributes,
      linkage=Linkage.Internal,
      callingConv=irSignature.callingConv,
      unnamedAddr=true
    )

    // Add our IR comment if one has been supplied
    for(irComment <- plannedFunction.irCommentOpt) {
      generatedFunction.entryBlock.comment(irComment)
    }

    val argsAreGcRoots = argumentsAreGcRoots(plannedFunction.signature)

    // Create a blank generation state with just our args
    val argTemps = plannedFunction.namedArguments.zip(argsAreGcRoots).foldLeft(LiveTemps()) {
      case (liveTemps, ((name, tempValue), gcRoot)) =>
        liveTemps + (tempValue -> CollectableIrValue(generatedFunction.argumentValues(name), gcRoot=gcRoot))
    }

    // Do we need to set up GC for this function?
    val (procStartBlock, gcSlotsOpt, gcCleanUpBlockOpt) = if (plannedFunction.signature.hasWorldArg) {
      val worldPtrIr = argTemps(ps.WorldPtrValue)

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
    }
    else {
      // No GC support
      (generatedFunction.entryBlock, None, None)
    }

    val startState = GenerationState(
      gcSlotsOpt=gcSlotsOpt,
      currentBlock=procStartBlock,
      currentAllocation=EmptyHeapAllocation(),
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
