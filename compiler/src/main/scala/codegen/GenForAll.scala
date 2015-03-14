package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.planner.{step => ps}
import llambda.llvmir._

object GenForAll {
  def apply(state : GenerationState, genGlobals : GenGlobals)(step : ps.ForAll) : GenResult = step match {
    case ps.ForAll(resultTemp, loopCountValueTemp, loopIndexValue, loopSteps, loopResultPred) =>
      val irFunction = state.currentBlock.function

      val previousBlock = state.currentBlock
      val rangeCheckBlock = irFunction.startChildBlock("forAllRangeCheck")
      val loopBodyBlock = irFunction.startChildBlock("forAllLoopBody")

      val loopContBlock = irFunction.startChildBlock("forAllLoopCont")

      val exitBlock = irFunction.startChildBlock("forAllExit")

      val indexType = IntegerType(64)
      val incedIndex = LocalVariable(irFunction.nameSource.allocate("incedIndex"), indexType)
      val loopCountIr = state.liveTemps(loopCountValueTemp)

      previousBlock.uncondBranch(rangeCheckBlock)

      // Find our loop index
      val loopIndexIr = rangeCheckBlock.phi("loopIndex")(
        PhiSource(IntegerConstant(indexType, 0), previousBlock),
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

      val loopResultPredIr = loopBodyResult match {
        case BlockTerminated(_) =>
          throw new InternalCompilerErrorException("ForAll loop body terminated")

        case loopEndState : GenerationState =>
          loopEndState.liveTemps(loopResultPred)
      }

      loopBodyBlock.condBranch(loopResultPredIr, loopContBlock, exitBlock)

      // It's impossible for this to wrap
      val wrapBehaviour = Set[WrapBehaviour](
        WrapBehaviour.NoSignedWrap,
        WrapBehaviour.NoUnsignedWrap
      )

      loopContBlock.add(incedIndex)(wrapBehaviour, loopIndexIr, IntegerConstant(indexType, 1))
      loopContBlock.uncondBranch(rangeCheckBlock)

      val resultIr = exitBlock.phi("forAllResult")(
        PhiSource(IntegerConstant(IntegerType(1), 1), rangeCheckBlock),
        PhiSource(IntegerConstant(IntegerType(1), 0), loopBodyBlock)
      )

      state.copy(currentBlock=exitBlock).withTempValue(
        resultTemp -> resultIr
      )
  }
}
