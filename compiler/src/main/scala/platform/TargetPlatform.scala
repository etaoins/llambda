package io.llambda.compiler.platform
import io.llambda

import llambda.compiler.{valuetype => vt}

/** Describes a target platform for code generation
  *
  * This may differ from the current platform during cross compilation
  */
trait TargetPlatform {
  val pointerBits: Int

  /** Indicates if the platform uses natural alignment
    *
    * If true, native types be aligned to a multiple of their sizes. If a more complex alignment is used this should be
    * set to false to allow LLVM to handle data alignment.
    */
  val usesNaturalAlignment: Boolean

  def bytesForType(valueType: vt.ValueType) = valueType match {
    // Use the + 7 to make sure we round up to the next byte boundary
    case intLikeType: vt.IntLikeType => (intLikeType.bits + 7) / 8
    case vt.Float => 4
    case vt.Double => 8
    case pointerType: vt.PointerType => pointerBits / 8
  }
}

/** Abstract ILP 32bit platform */
trait AbstractILP32 extends TargetPlatform {
  val pointerBits = 32
  val usesNaturalAlignment = true
}

/** Abstract 64bit platform */
trait Abstract64 extends TargetPlatform {
  val pointerBits = 64

  val usesNaturalAlignment = true
}

/** Abstract 64bit platform using the LP64 data model */
trait AbstractLP64 extends Abstract64 {
}

/** Abstract 64bit platform using the LLP64 data model */
trait AbstractLLP64 extends Abstract64 {
}

/** Abstract modern POSIX platform
  *
  * Recent MacOS X, FreeBSD and Linux versions are actively supported and tested.
  * Other POSIX platorms supported by LLVM will likely work.
  */
trait AbstractPosix extends TargetPlatform {
}

/** Abstract Microsoft Windows-based platform
  *
  * Windows is currently unsupported. This in included for completeness
  */
trait AbstractWindows extends TargetPlatform {
}

/** Abstract big-endian platform */
trait AbstractBigEndian extends TargetPlatform {
}

/** Abstract little-endian platform */
trait AbstractLittleEndian extends TargetPlatform {
}

/** Modern big-endian 64bit POSIX platform */
object Posix64BE extends AbstractPosix with AbstractLP64 with AbstractBigEndian

/** Modern little-endian 64bit POSIX platform */
object Posix64LE extends AbstractPosix with AbstractLP64 with AbstractLittleEndian

/** Modern big-endian 32bit POSIX platform */
object Posix32BE extends AbstractPosix with AbstractILP32 with AbstractBigEndian

/** Modern little-endian 32bit POSIX platform */
object Posix32LE extends AbstractPosix with AbstractILP32 with AbstractLittleEndian

/** Microsoft's Win64 platform */
object Win64 extends AbstractWindows with AbstractLLP64 with AbstractLittleEndian
/** Microsoft's Win32 platform */
object Win32 extends AbstractWindows with AbstractILP32 with AbstractLittleEndian
