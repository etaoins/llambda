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
    arguments=List(IrFunction.Argument(PointerType(IntegerType(8)))),
    attributes=Set(IrFunction.NoUnwind)
  )

  def apply(state : GenerationState)(boxStep : ps.BoxValue, nativeValue : IrValue) : IrValue = boxStep match {
    case _ : ps.BoxBoolean =>
      state.currentBlock.select("boxedBool")(nativeValue, GlobalDefines.trueIrValue, GlobalDefines.falseIrValue)

    case ps.BoxExactInteger(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedIntCons = allocation.genTypedPointer(block)(allocIndex, ct.ExactIntegerCell) 
      ct.ExactIntegerCell.genStoreToValue(block)(nativeValue, boxedIntCons)

      boxedIntCons
    
    case ps.BoxInexactRational(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedRationalCons = allocation.genTypedPointer(block)(allocIndex, ct.InexactRationalCell) 
      ct.InexactRationalCell.genStoreToValue(block)(nativeValue, boxedRationalCons)

      boxedRationalCons

    case ps.BoxCharacter(_, allocTemp, allocIndex, _) =>
      val block = state.currentBlock
      val allocation = state.liveAllocations(allocTemp)

      val boxedCharCons = allocation.genTypedPointer(block)(allocIndex, ct.CharacterCell) 
      ct.CharacterCell.genStoreToUnicodeChar(block)(nativeValue, boxedCharCons)

      boxedCharCons
  }
}


