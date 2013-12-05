package llambda.platform

import llambda.{valuetype => vt}

/** Describes a target platform for code generation 
  *
  * This may differ from the current platform during cross compilation 
  */
trait TargetPlatform {
  val pointerBits : Int

  val shortType  : vt.IntType
  val ushortType : vt.IntType
  val intType    : vt.IntType
  val uintType   : vt.IntType
  val longType   : vt.IntType

  val sizeType  : vt.IntType
  val wcharType : vt.IntType
}

/** Abstract ILP 32bit platform */
trait AbstractILP32 extends TargetPlatform {
  val pointerBits = 32

  val shortType  = vt.Int16
  val ushortType = vt.UInt16
  val intType    = vt.Int32
  val uintType   = vt.UInt32
  val longType   = vt.Int32
  
  val sizeType  = vt.Int32
}

/** Abstract 64bit platform */
trait Abstract64 extends TargetPlatform {
  val pointerBits = 64
  
  val shortType  = vt.Int16
  val ushortType = vt.UInt16
  val intType    = vt.Int32
  val uintType   = vt.UInt32

  val sizeType  = vt.Int64
}

/** Abstract 64bit platform using the LP64 data model */
trait AbstractLP64 extends Abstract64 {
  val longType   = vt.Int64
}

/** Abstract 64bit platform using the LLP64 data model */
trait AbstractLLP64 extends Abstract64 {
  val longType   = vt.Int32
}

/** Abstract modern POSIX platform 
  *
  * Recent MacOS X, FreeBSD and Linux versions are actively supported and tested.
  * Other POSIX platorms supported by LLVM will likely work.
  */
trait AbstractPosix extends TargetPlatform {
  // POSIX typically uses 32bit wchar_t
  val wcharType = vt.UInt32
}

/** Abstract Microsoft Windows-based platform
  *
  * Windows is currently unsupported. This in included for completeness
  */
trait AbstractWindows extends TargetPlatform {
  // Windows uses 16 wchar_t for historical reasons
  val wcharType = vt.UInt16
}

/** Modern 64bit POSIX platform */
object Posix64 extends AbstractPosix with AbstractLP64
/** Modern 32bit POSIX platform */
object Posix32 extends AbstractPosix with AbstractILP32

/** Microsoft's Win64 platform */
object Win64 extends AbstractWindows with AbstractLLP64
/** Microsoft's Win32 platform */
object Win32 extends AbstractWindows with AbstractILP32
