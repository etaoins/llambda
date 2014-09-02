package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.planner.{step => ps}
import llambda.llvmir._

object GenForAll {
  def apply(state : GenerationState, genGlobals : GenGlobals)(step : ps.ForAll) : GenResult = step match {
    case ps.ForAll(resultTemp, loopCountValueTemp, loopIndexValue, loopSteps, loopResultPred) =>
      val irFunction = state.currentBlock.function

      // See if this is an unconditional loop so we can simplify our IR
      // This requires that the loop condition is outside the loop body
      val isUncondLoop = state.liveTemps.get(loopResultPred) match {
        case Some(IntegerConstant(IntegerType(1), 1)) =>
          true

        case _ => 
          false
      }

      val previousBlock = state.currentBlock
      val rangeCheckBlock = irFunction.startChildBlock("forAllRangeCheck")
      val loopBodyBlock = irFunction.startChildBlock("forAllLoopBody")

      val loopContBlock = if (isUncondLoop) {
        // We don't need a continuation block if we're in an unconditional loop
        loopBodyBlock
      }
      else {
        irFunction.startChildBlock("forAllLoopCont")
      }

      val exitBlock = irFunction.startChildBlock("forAllExit")

      val indexType = IntegerType(32)
      val incedIndex = LocalVariable(irFunction.nameSource.allocate("incedIndex"), indexType)
      val loopCountIr = state.liveTemps(loopCountValueTemp)

      previousBlock.uncondBranch(rangeCheckBlock)

      // Find our loop index
      val loopIndexIr = rangeCheckBlock.phi("loopIndex")(
        PhiSource(IntegerConstant(IntegerType(32), 0), previousBlock),
        PhiSource(incedIndex, loopContBlock)
      )

      val indexExhaustedIr = rangeCheckBlock.icmp("indexExhausted")(
        compareCond=IComparisonCond.GreaterThanEqual,
        signed=Some(false),
        val1=loopIndexIr,
        val2=loopCountIr
      )

      rangeCheckBlock.condBranch(indexExhaustedIr, exitBlock, loopBodyBlock)
      
      val loopBodyStartState = state.copy(
        currentBlock=loopBodyBlock
      ).withTempValue(loopIndexValue -> loopIndexIr)

      val loopBodyResult = GenPlanSteps(loopBodyStartState, genGlobals)(loopSteps)

      if (!isUncondLoop) {
        val loopResultPredIr = loopBodyResult match {
          case BlockTerminated(_) =>
            throw new InternalCompilerErrorException("ForAll loop body terminated") 

          case loopEndState : GenerationState =>
            loopEndState.liveTemps(loopResultPred)
        }

        loopBodyBlock.condBranch(loopResultPredIr, loopContBlock, exitBlock) 
      }

      // It's impossible for this to wrap
      val wrapBehaviour = Set[WrapBehaviour](
        WrapBehaviour.NoSignedWrap,
        WrapBehaviour.NoUnsignedWrap
      )

      loopContBlock.add(incedIndex)(wrapBehaviour, loopIndexIr, IntegerConstant(indexType, 1))
      loopContBlock.uncondBranch(rangeCheckBlock)

      val resultIr = if (isUncondLoop) {
        IntegerConstant(IntegerType(1), 1)
      }
      else {
        exitBlock.phi("forAllResult")(
          PhiSource(IntegerConstant(IntegerType(1), 1), rangeCheckBlock),
          PhiSource(IntegerConstant(IntegerType(1), 0), loopBodyBlock)
        )
      }

      state.copy(currentBlock=exitBlock).withTempValue(
        resultTemp -> resultIr
      )
  }
}
