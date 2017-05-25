package io.llambda.compiler.codegen


object SharedByteArrayHash {
  private val uninitialisedHashValue = 0
  private val uninitialisedHashRemapValue = 0x86b2bb0d

  private val FNV1APrime = 0x1000193
  private val FNV1AOffsetBasis = -0x7EE3623B // 0x811C9DC5 as signed 32bit

  def fromBytes(bytes: Seq[Byte]): Int = {
    val hash = bytes.foldLeft(FNV1AOffsetBasis) { (hash, byte) => (hash ^ (byte & 0xff)) * FNV1APrime }
    if (hash == uninitialisedHashValue) uninitialisedHashRemapValue else hash
  }
}
