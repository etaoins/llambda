package io.llambda.compiler.celltype
import io.llambda

import llambda.llvmir._
import io.llambda.compiler.InternalCompilerErrorException

private object ArrayElementsForIrType {
  def apply(irType : IrType) : Int = irType match {
    case ArrayType(elements, _) =>
      elements

    case _ =>
      throw new InternalCompilerErrorException("Expected cell member to be array")
  }
}

object SymbolCellConstants {
  /** Value that inlineByteLength takes when the symbol is stored on the heap */
  val heapSymbolInlineByteLength = 255

  /** Maximum number of bytes that an can be stored in an inline symbol */
  lazy val maximumInlineSymbolBytes =
    ArrayElementsForIrType(InlineSymbolCell.inlineDataIrType)
}

object StringCellConstants {
  /** Value that inlineByteLength takes when the string is stored on the heap */
  val heapStringInlineByteLength = 255

  /** Maximum number of bytes that an can be stored in an inline string */
  lazy val maximumInlineStringBytes =
    ArrayElementsForIrType(InlineStringCell.inlineDataIrType)
}
