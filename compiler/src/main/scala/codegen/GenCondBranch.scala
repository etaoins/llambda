package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._


private[codegen] object GenCondBranch {
  def apply(state: GenerationState, genGlobals: GenGlobals)(step: ps.CondBranch): GenResult = step match {
    case ps.CondBranch(testTemp, trueSteps, falseSteps, valuePhis) =>
      val testIr = state.liveTemps(testTemp)

      // Make two blocks
      val irFunction = state.currentBlock.function
      val trueStartBlock = irFunction.startChildBlock("condTrue")
      val falseStartBlock = irFunction.startChildBlock("condFalse")

      // Branch!
      state.currentBlock.condBranch(testIr, trueStartBlock, falseStartBlock)

      // Continue generation down both branches after splitting our state
      val trueStartState = state.copy(
        currentBlock=trueStartBlock
      )

      val falseStartState = state.copy(
        currentBlock=falseStartBlock
      )

      val trueResult = GenPlanSteps(trueStartState, genGlobals)(trueSteps)
      val falseResult = GenPlanSteps(falseStartState, genGlobals)(falseSteps)

      (trueResult, falseResult) match {
        case (BlockTerminated, BlockTerminated) =>
          // Both branches terminated
          BlockTerminated

        case (trueEndState: GenerationState, BlockTerminated) =>
          // Only true terminated
          val state = trueEndState

          valuePhis.foldLeft(state) { case (state, valuePhi) =>
            val trueValueIr = trueEndState.liveTemps(valuePhi.trueValue)
            state.withTempValue(valuePhi.result -> trueValueIr)
          }

        case (BlockTerminated, falseEndState: GenerationState) =>
          // Only false terminated
          val state = falseEndState

          valuePhis.foldLeft(state) { case (state, valuePhi) =>
            val falseValueIr = falseEndState.liveTemps(valuePhi.falseValue)
            state.withTempValue(valuePhi.result -> falseValueIr)
          }

        case (trueEndState: GenerationState, falseEndState: GenerationState) =>
          // Neither branch terminated - we need to phi

          val trueEndBlock = trueEndState.currentBlock
          val falseEndBlock = falseEndState.currentBlock

          // Make a final block
          val irFunction = state.currentBlock.function

          val phiBlock = irFunction.startChildBlock("condPhi")
          trueEndBlock.uncondBranch(phiBlock)
          falseEndBlock.uncondBranch(phiBlock)

          val phiBlockState = state.copy(currentBlock=phiBlock)

          valuePhis.foldLeft(phiBlockState) { case (state, valuePhi) =>
            val trueResultIrValue = trueEndState.liveTemps(valuePhi.trueValue)
            val falseResultIrValue = falseEndState.liveTemps(valuePhi.falseValue)

            val phiResultIr = phiBlock.phi("condPhiResult")(
              PhiSource(trueResultIrValue, trueEndBlock),
              PhiSource(falseResultIrValue, falseEndBlock)
            )

            state.withTempValue(valuePhi.result -> phiResultIr)
          }
      }
  }
}
