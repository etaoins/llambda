package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

/** Helper functions related to the World object */
object WorldValue extends StructureValue("world") {
  val cellPointerIrType = PointerType(UserDefinedType("cell"))

  val shadowStackHeadField = StructureField(
    name="shadowStackHead",
    index=0,
    irType=PointerType(ShadowStackEntryHeaderValue.irType),
    tbaaNode=NumberedMetadata(0)
  )

  val allocNextField = StructureField(
    name="allocNext",
    index=1,
    irType=cellPointerIrType,
    tbaaNode=NumberedMetadata(1)
  )

  val allocEndField = StructureField(
    name="allocEnd",
    index=2,
    irType=cellPointerIrType,
    tbaaNode=NumberedMetadata(2)
  )
  
  def genPointerToShadowStackHead = genPointerToField(shadowStackHeadField)_
  def genLoadFromShadowStackHead = genLoadFromField(shadowStackHeadField)_
  def genStoreToShadowStackHead = genStoreToField(shadowStackHeadField)_
  
  def genPointerToAllocNext = genPointerToField(allocNextField)_
  def genLoadFromAllocNext = genLoadFromField(allocNextField)_
  def genStoreToAllocNext = genStoreToField(allocNextField)_

  def genPointerToAllocEnd = genPointerToField(allocEndField)_
  def genLoadFromAllocEnd = genLoadFromField(allocEndField)_
}
