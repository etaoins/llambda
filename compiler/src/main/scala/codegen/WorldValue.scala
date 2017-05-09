package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

/** Helper functions related to the World object */
object WorldValue extends StructureValue("world") {
  val cellPointerIrType = PointerType(UserDefinedType("cell"))

  val allocNextField = StructureField(
    name="allocNext",
    index=0,
    irType=cellPointerIrType,
    tbaaNode=NumberedMetadata(2)
  )

  val allocEndField = StructureField(
    name="allocEnd",
    index=1,
    irType=cellPointerIrType,
    tbaaNode=NumberedMetadata(3)
  )

  def genPointerToAllocNext = genPointerToField(allocNextField)_
  def genLoadFromAllocNext = genLoadFromField(allocNextField)_
  def genStoreToAllocNext = genStoreToField(allocNextField)_

  def genPointerToAllocEnd = genPointerToField(allocEndField)_
  def genLoadFromAllocEnd = genLoadFromField(allocEndField)_
}
