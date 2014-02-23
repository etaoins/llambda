package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

/**
 * Helper functions related to the World object
 *
 * This interface is modelled after the CellType objects 
 */
object WorldValue {
  val irType = UserDefinedType("world")
  val cellPointerIrType = PointerType(UserDefinedType("cell"))

  private case class WorldField(name : String, index : Int, irType : FirstClassType)

  private val allocNextField = WorldField("allocNext", 0, cellPointerIrType)
  private val allocEndField = WorldField("allocEnd", 1, cellPointerIrType)
  
  private def genPointerToField(field : WorldField)(block : IrBlockBuilder)(valueCell : IrValue) : IrValue = {
    block.getelementptr(field.name + "Ptr")(
      elementType=field.irType,
      basePointer=valueCell,
      indices=List(0, field.index).map(IntegerConstant(IntegerType(32), _)),
      inbounds=true
    )
  }
  
  private def genStoreToField(field : WorldField)(block : IrBlockBuilder)(toStore : IrValue, valueCell : IrValue)  {
    val pointer = genPointerToField(field)(block)(valueCell)
    block.store(toStore, pointer, tbaaIndex=Some(field.index))
  }

  private def genLoadFromField(field : WorldField)(block : IrBlockBuilder)(valueCell : IrValue) : IrValue = {
    val pointer = genPointerToField(field)(block)(valueCell)
    block.load(field.name)(pointer, tbaaIndex=Some(field.index))
  }

  def genPointerToAllocNext = genPointerToField(allocNextField)_
  def genLoadFromAllocNext = genLoadFromField(allocNextField)_
  def genStoreToAllocNext = genStoreToField(allocNextField)_

  def genPointerToAllocEnd = genPointerToField(allocEndField)_
  def genLoadFromAllocEnd = genLoadFromField(allocEndField)_
}
