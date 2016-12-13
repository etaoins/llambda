package io.llambda.llvmir

private[llvmir] object BytesToIrString {
  /** Converts a sequence of bytes to an LLVM IR string */
  def apply(stringBytes: Seq[Byte]): String = "\"" + (stringBytes flatMap {
    case backslash if backslash == 92 =>
      """\\"""
    case doubleQuote if doubleQuote == 34 =>
      "\\22"
    case printable if ((printable >= 32) && (printable <= 126)) =>
      printable.toChar.toString
    case unprintable =>
      f"\\$unprintable%02X"
  }).mkString + "\""
}


