package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.platform.TargetPlatform

private[codegen] object GenFunction {
  private def stepsGcManagedTemps(steps : List[ps.Step]) : Set[ps.TempValue] = 
    (steps.flatMap { step =>
      step.outputValues.filter(_.isGcManaged) ++
        (step match {
          case branch @ ps.CondBranch(_, _, trueSteps, _, falseSteps, _) =>
            stepsGcManagedTemps(trueSteps) ++ stepsGcManagedTemps(falseSteps)

          case _ =>
            Set()
        })
    }).toSet

  private def functionGcManagedTemps(plannedFunction : planner.PlannedFunction) : Set[ps.TempValue] = {
    val gcManagedArgs = plannedFunction.namedArguments.map(_._2).filter(_.isGcManaged).toSet
    val gcManagedBodyTemps = stepsGcManagedTemps(plannedFunction.steps)

    gcManagedArgs ++ gcManagedBodyTemps
  }

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
      attributes=Set(IrFunction.NoUnwind),
      gc=Some("shadow-stack")) 

    // Create a blank generation state with just our args
    val argTemps = (plannedFunction.namedArguments map { case (name, tempValue) =>
      (tempValue, generatedFunction.argumentValues(name))
    }).toMap
    
    // Allocate rooted GC slots for our values
    val gcManagedTemps = functionGcManagedTemps(plannedFunction)
    val gcSlots = GenGcSlots(module, generatedFunction.entryBlock)(gcManagedTemps)

    val startState = GenerationState(
      module=module,
      gcSlots=gcSlots,
      currentBlock=generatedFunction.entryBlock,
      liveTemps=argTemps)

    // Generate our steps
    GenPlanSteps(startState, plannedSymbols, typeGenerator)(plannedFunction.steps)
      
    // Define the function
    module.defineFunction(generatedFunction)
  }
}
