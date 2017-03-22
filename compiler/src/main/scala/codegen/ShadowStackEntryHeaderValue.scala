package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._

object ShadowStackEntryHeaderValue extends StructureValue("shadowStackEntryHeader") {
  val nextField = StructureField(
    name="next",
    index=0,
    irType=PointerType(UserDefinedType("shadowStackEntryHeader")),
    tbaaNode=NumberedMetadata(4)
  )

  val cellCountField = StructureField(
    name="cellCount",
    index=1,
    irType=IntegerType(32),
    tbaaNode=NumberedMetadata(5)
  )

  def genPointerToNext = genPointerToField(nextField)_
  def genLoadFromNext = genLoadFromField(nextField)_
  def genStoreToNext = genStoreToField(nextField)_

  def genPointerToCellCount = genPointerToField(cellCountField)_
  def genLoadFromCellCount = genLoadFromField(cellCountField)_
  def genStoreToCellCount = genStoreToField(cellCountField)_
}
