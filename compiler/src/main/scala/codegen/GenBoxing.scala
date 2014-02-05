package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GenBoxing {
  private val llibyStringFromUtf8Decl = IrFunctionDecl(
    result=IrFunction.Result(PointerType(ct.StringCell.irType)),
    name="_lliby_string_from_utf8",
    arguments=List(IrFunction.Argument(PointerType(IntegerType(8))))
  )

  def apply(state : GenerationState)(boxStep : ps.BoxValue, nativeValue : IrValue) : (GenerationState, IrValue) = boxStep match {
    case _ : ps.BoxBoolean =>
      val irValue = state.currentBlock.select("boxedBool")(
        nativeValue,
        GlobalDefines.trueIrValue,
        GlobalDefines.falseIrValue
      )

      (state, irValue)

    case _ : ps.BoxExactInteger =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedIntCons) = allocation.consumeCells(block)(1, ct.ExactIntegerCell) 
      ct.ExactIntegerCell.genStoreToValue(block)(nativeValue, boxedIntCons)

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedIntCons)
    
    case _ : ps.BoxInexactRational =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedRationalCons) = allocation.consumeCells(block)(1, ct.InexactRationalCell) 
      ct.InexactRationalCell.genStoreToValue(block)(nativeValue, boxedRationalCons)

      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedRationalCons)

    case _ : ps.BoxCharacter =>
      val block = state.currentBlock
      val allocation = state.currentAllocation

      val (newAllocation, boxedCharCons) = allocation.consumeCells(block)(1, ct.CharacterCell) 
      ct.CharacterCell.genStoreToUnicodeChar(block)(nativeValue, boxedCharCons)
      
      val newState = state.copy(
        currentAllocation=newAllocation
      )

      (newState, boxedCharCons)
  }
}


