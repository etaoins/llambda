package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenCalcProperListLength {
  private val listElementPtrType = PointerType(ct.ListElementCell.irType)
  private val pairPtrType = PointerType(ct.PairCell.irType)
  private val emptyListPtrType = PointerType(ct.EmptyListCell.irType)

  def apply(initialState: GenerationState)(listHead: IrValue): (GenerationState, IrValue) = {
    val prevBlock = initialState.currentBlock
    val function = prevBlock.function

    // Build IR values for our phi
    val currentCounterIr = LocalVariable(
      name=function.nameSource.allocate("currentCounter"),
      irType=IntegerType(32)
    )

    val incedCounterIr = LocalVariable(
      name=function.nameSource.allocate("incedCounter"),
      irType=IntegerType(32)
    )

    val currentListElIr = LocalVariable(
      name=function.nameSource.allocate("currentListEl"),
      irType=listElementPtrType
    )

    val nextListElIr = LocalVariable(
      name=function.nameSource.allocate("nextListEl"),
      irType=listElementPtrType
    )

    // Build our calc block
    val countBlock = function.startChildBlock("lengthCount")

    // Build our count continuation block
    val continueCountBlock = function.startChildBlock("lengthContinue")

    // Create our exit block
    val countDoneBlock = function.startChildBlock("lengthComplete")

    // Branch to the count block
    prevBlock.uncondBranch(countBlock)

    // Create our phi values
    countBlock.phi(currentCounterIr)(
      PhiSource(IntegerConstant(IntegerType(32), 0), prevBlock),
      PhiSource(incedCounterIr, continueCountBlock)
    )

    countBlock.phi(currentListElIr)(
      PhiSource(listHead, prevBlock),
      PhiSource(nextListElIr, continueCountBlock)
    )

    // Compare the list element to the empty list
    val emptyListCastIr = countBlock.bitcastTo("emptyListCast")(currentListElIr, emptyListPtrType)
    val isEmptyListIr = countBlock.icmp("isEmptyList")(IComparisonCond.Equal, None, emptyListCastIr, GlobalDefines.emptyListIrValue)

    // Jump to the exit if we've reached the end
    countBlock.condBranch(isEmptyListIr, countDoneBlock, continueCountBlock)

    // In the continue block load the car
    val pairCastIr = continueCountBlock.bitcastTo("pairCast")(currentListElIr, pairPtrType)
    val cdrIr = ct.PairCell.genLoadFromCdr(continueCountBlock)(pairCastIr)

    // And increment our count
    val wrapBehaviour = Set(WrapBehaviour.NoSignedWrap, WrapBehaviour.NoUnsignedWrap): Set[WrapBehaviour]
    continueCountBlock.add(incedCounterIr)(wrapBehaviour, currentCounterIr, IntegerConstant(IntegerType(32), 1))

    // Cast it to a list element
    val listElementCastIr = continueCountBlock.bitcastTo(nextListElIr)(cdrIr, listElementPtrType)

    // Jump back to the beginning of the loop
    continueCountBlock.uncondBranch(countBlock)

    // Deal with the finished count
    val finalState = initialState.copy(
      currentBlock=countDoneBlock
    )

    return (finalState, currentCounterIr)
  }
}
