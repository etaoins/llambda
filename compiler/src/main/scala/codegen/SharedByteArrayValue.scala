package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._


object SharedByteArrayValue {
  // Maximum value of a 32bit unsigned integer
  private val sharedConstantRefCount = (math.pow(2, 32) - 1).toLong

  val irType = UserDefinedType("sharedByteArray")

  val refCountIrType = IntegerType(32)

  val cachedHashValueIrType = IntegerType(32)

  val dataIrType = PointerType(IntegerType(8))
  val dataTbaaNode = NumberedMetadata(5)

  /** Creates a constant SharedByteArray using a string encoded as UTF-8 */
  def createUtf8StringConstant(utf8Data: Seq[Byte]): StructureConstant = {
    StructureConstant(List(
      IntegerConstant(refCountIrType, sharedConstantRefCount),
      IntegerConstant(cachedHashValueIrType, SharedByteArrayHash.fromBytes(utf8Data)),
      StringConstant(utf8Data)
    ))
  }

  /** Creates a onstant SharedByteArray from a sequence of bytes */
  def createArrayConstant(elements: Seq[Byte]): StructureConstant = {
    val elementIrs = elements.map { byteValue =>
      // Manually zero extend the bytes to prevent bytes >127 from being rendered as negative numbers. This is purely
      // aesthetic
      val longValue = if (byteValue < 0) byteValue.toLong + 256 else byteValue.toLong

      IntegerConstant(IntegerType(8), longValue)
    }

    StructureConstant(List(
      IntegerConstant(refCountIrType, sharedConstantRefCount),
      IntegerConstant(cachedHashValueIrType, SharedByteArrayHash.fromBytes(elements)),
      ArrayConstant(IntegerType(8), elementIrs)
    ))
  }

  def genPointerToDataByte(block: IrBlockBuilder)(structureValue: IrValue, indexIr: IrValue) = {
    val gepIndices = List(0, 2).map(IntegerConstant(IntegerType(32), _)) :+ indexIr
    block.getelementptr("bytePtr")(IntegerType(8), structureValue, gepIndices, inbounds=true)
  }

  def genLoadFromDataByte(block: IrBlockBuilder)(structureValue: IrValue, indexIr: IrValue) = {
    val bytePtr = genPointerToDataByte(block)(structureValue, indexIr)
    block.load("byte")(bytePtr, metadata=Map("tbaa" -> dataTbaaNode))
  }
}
