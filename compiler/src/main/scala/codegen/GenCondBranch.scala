package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._

private[codegen] object GenCondBranch {
  import Implicits._

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
            state.withTempValue(valuePhi.result -> trueValueIr, gcRoot=trueValueIr.gcRoot)
          }

        case (BlockTerminated, falseEndState: GenerationState) =>
          // Only false terminated
          val state = falseEndState

          valuePhis.foldLeft(state) { case (state, valuePhi) =>
            val falseValueIr = falseEndState.liveTemps(valuePhi.falseValue)
            state.withTempValue(valuePhi.result -> falseValueIr, falseValueIr.gcRoot)
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

          // Find our common temp values in sorted order to ensure we generate stable IR
          val sortedTrueLiveTemps = trueEndState.liveTemps.tempValueToCiv.toSeq.sortBy(_._2.toIr).map(_._1)
          val sortedCommonLiveTemps = sortedTrueLiveTemps.filter(falseEndState.liveTemps.tempValueToCiv.contains)

          // Phi any values that have diverged across branches
          val newTempValueToCiv = sortedCommonLiveTemps map { liveTemp =>
            val trueValueIrValue = trueEndState.liveTemps(liveTemp)
            val falseValueIrValue = falseEndState.liveTemps(liveTemp)

            if (trueValueIrValue == falseValueIrValue) {
              // This is the same in both branches which means it came from our original state
              (liveTemp -> trueValueIrValue)
            }
            else {
              // This has diverged due to e.g. GC having happened in one branch
              val phiValueIr = phiBlock.phi("condPhiValue")(PhiSource(trueValueIrValue, trueEndBlock), PhiSource(falseValueIrValue, falseEndBlock))
              val phiValueCiv = CollectableIrValue(phiValueIr, gcRoot=trueValueIrValue.gcRoot)

              (liveTemp -> phiValueCiv)
            }
          }

          // Make sure we preserve pointer identities or else the identity count will explode
          val phiedLiveValuesState = state.copy(
            currentBlock=phiBlock,
            liveTemps=state.liveTemps.copy(tempValueToCiv=newTempValueToCiv.toMap)
          )

          valuePhis.foldLeft(phiedLiveValuesState) { case (state, valuePhi) =>
            val trueResultIrValue = trueEndState.liveTemps(valuePhi.trueValue)
            val falseResultIrValue = falseEndState.liveTemps(valuePhi.falseValue)

            val phiResultIr = phiBlock.phi("condPhiResult")(
              PhiSource(trueResultIrValue, trueEndBlock),
              PhiSource(falseResultIrValue, falseEndBlock)
            )

            val gcRoot = trueResultIrValue.gcRoot || falseResultIrValue.gcRoot
            state.withTempValue(valuePhi.result -> phiResultIr, gcRoot=gcRoot)
          }
      }
  }
}
