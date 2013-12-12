package io.llambda.compiler.platform
import io.llambda

import llambda.compiler.{valuetype => vt}

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

  /** Indicates if the platform uses natural alignment
    *
    * If true, native types be aligned to a multiple of their sizes. If a more
    * complex alignment is used this should be set to false to allow LLVM to
    * handle data alignment.
    */
  val usesNaturalAlignment : Boolean

  protected val dataModelFeature : String
  protected val osFamilyFeature : String

  def platformFeatures : Set[String] =
    Set(dataModelFeature, osFamilyFeature)

  def bytesForType(valueType : vt.ValueType) = valueType match {
    case intLikeType : vt.IntLikeType => intLikeType.bits / 8
    case vt.Float => 4
    case vt.Double => 8
    case pointerType : vt.PointerType => pointerBits / 8
  }
}

/** Abstract ILP 32bit platform */
trait AbstractILP32 extends TargetPlatform {
  val pointerBits = 32

  val shortType  = vt.Int16
  val ushortType = vt.UInt16
  val intType    = vt.Int32
  val uintType   = vt.UInt32
  val longType   = vt.Int32
  
  val sizeType  = vt.UInt32

  val usesNaturalAlignment = true

  protected val dataModelFeature = "ilp32"
}

/** Abstract 64bit platform */
trait Abstract64 extends TargetPlatform {
  val pointerBits = 64
  
  val shortType  = vt.Int16
  val ushortType = vt.UInt16
  val intType    = vt.Int32
  val uintType   = vt.UInt32

  // XXX: size_t is unsigned but we can't represent unsigned 64bit ints
  val sizeType  = vt.Int64
  
  val usesNaturalAlignment = true
}

/** Abstract 64bit platform using the LP64 data model */
trait AbstractLP64 extends Abstract64 {
  val longType   = vt.Int64

  protected val dataModelFeature = "lp64"
}

/** Abstract 64bit platform using the LLP64 data model */
trait AbstractLLP64 extends Abstract64 {
  val longType   = vt.Int32
  
  protected val dataModelFeature = "llp64"
}

/** Abstract modern POSIX platform 
  *
  * Recent MacOS X, FreeBSD and Linux versions are actively supported and tested.
  * Other POSIX platorms supported by LLVM will likely work.
  */
trait AbstractPosix extends TargetPlatform {
  // POSIX typically uses 32bit wchar_t
  val wcharType = vt.UInt32

  protected val osFamilyFeature = "posix"
}

/** Abstract Microsoft Windows-based platform
  *
  * Windows is currently unsupported. This in included for completeness
  */
trait AbstractWindows extends TargetPlatform {
  // Windows uses 16 wchar_t for historical reasons
  val wcharType = vt.UInt16
  
  protected val osFamilyFeature = "windows"
}

/** Modern 64bit POSIX platform */
object Posix64 extends AbstractPosix with AbstractLP64
/** Modern 32bit POSIX platform */
object Posix32 extends AbstractPosix with AbstractILP32

/** Microsoft's Win64 platform */
object Win64 extends AbstractWindows with AbstractLLP64
/** Microsoft's Win32 platform */
object Win32 extends AbstractWindows with AbstractILP32
