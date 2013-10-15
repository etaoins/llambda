package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

object LiveNumeric {
  def genCheckedUnboxing(initialState : GenerationState)(boxedValue : IrValue, targetType : nfi.FpType) : (GenerationState, IrValue) = {
    // Build all of our blocks
    val exactIntBlock = initialState.currentBlock.startChildBlock("isExactInt") 
    val notExactIntBlock = initialState.currentBlock.startChildBlock("notExactInt")
    val inexactRationalBlock = notExactIntBlock.startChildBlock("isInexactRational") 
    val failBlock = notExactIntBlock.startChildBlock("notNumeric")
    val phiBlock = initialState.currentBlock.startChildBlock("numericUnboxPhi")

    // See if we're boxed exact integer
    bt.BoxedExactInteger.genTypeCheck(initialState.currentBlock)(boxedValue, exactIntBlock, notExactIntBlock)
    // Then fall back to inexact rational
    bt.BoxedInexactRational.genTypeCheck(notExactIntBlock)(boxedValue, inexactRationalBlock, failBlock) 

    // Unbox on the exact int side
    val exactIntBoxedValue = bt.BoxedExactInteger.genPointerBitcast(exactIntBlock)(boxedValue)
    val unboxedIntValue = LiveExactInteger.genFpUnboxing(exactIntBlock)(exactIntBoxedValue, targetType)
    exactIntBlock.uncondBranch(phiBlock)

    // Unbox on the inexact rational side
    val rationalBoxedValue = bt.BoxedInexactRational.genPointerBitcast(inexactRationalBlock)(boxedValue)
    val unboxedRationalValue = LiveInexactRational.genUnboxing(inexactRationalBlock)(rationalBoxedValue, targetType)
    inexactRationalBlock.uncondBranch(phiBlock)

    // And error out on the failure side 
    val errorName = s"unboxNumericFailed"
    val errorMessage = s"Attempted to unbox a non-numeric value to a floating point native value"
    GenFatalError(initialState.module, failBlock)(errorName, errorMessage)

    // Collect the boxed values from the unboxing branches
    val phiValue = phiBlock.phi("unboxedNumeric")(PhiSource(unboxedIntValue, exactIntBlock), PhiSource(unboxedRationalValue, inexactRationalBlock))

    val newState = initialState.copy(currentBlock=phiBlock)

    (newState, phiValue)
  }
}
