package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._


object SharedByteArrayValue {
  // Maximum value of a 32bit unsigned integer
  private val sharedConstantRefCount = (math.pow(2, 32) - 1).toLong

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

  /** Creates a onstant SharedByteArray from an array of shorts
    *
    * Shorts are used to emulate unsigned bytes. They are expected to be in the range 0-255
    */
  def createShortArrayConstant(elements: Seq[Short]): StructureConstant = {
    val elementIrs = elements.map(IntegerConstant(IntegerType(8), _))

    StructureConstant(List(
      IntegerConstant(refCountIrType, sharedConstantRefCount),
      IntegerConstant(cachedHashValueIrType, SharedByteArrayHash.fromShorts(elements)),
      ArrayConstant(IntegerType(8), elementIrs)
    ))
  }

  def genPointerToDataByte(block: IrBlockBuilder)(structureValue: IrValue, indexIr: IrValue) = {
    val gepIndices = List(0, 2).map(IntegerConstant(IntegerType(32), _)) :+ indexIr
    block.getelementptr("bytePtr")(IntegerType(8), structureValue, gepIndices, inbounds=true)
  }
}
