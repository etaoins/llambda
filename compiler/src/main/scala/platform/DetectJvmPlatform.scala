package io.llambda.compiler.platform
import io.llambda

import java.nio.ByteOrder
import java.nio.ByteOrder.{BIG_ENDIAN, LITTLE_ENDIAN}

object DetectJvmPlatform {
  def apply() : Option[TargetPlatform] = {
    val dataModelBits = System.getProperty("sun.arch.data.model").toInt
    val isWindows = util.Properties.isWin

    (dataModelBits, isWindows, ByteOrder.nativeOrder) match {
      case (64, false, BIG_ENDIAN)    => Some(Posix64BE)
      case (64, false, LITTLE_ENDIAN) => Some(Posix64LE)
      case (32, false, BIG_ENDIAN)    => Some(Posix32BE)
      case (32, false, LITTLE_ENDIAN) => Some(Posix32LE)

      // Consumer Windows only comes in little-endian
      // Xbox 360 is big-endian but it's doubtful a public LLVM release will  ever generate code for it
      case (64, true, LITTLE_ENDIAN)  => Some(Win64)
      case (32, true, LITTLE_ENDIAN)  => Some(Win32)
      case _ => None
    }
  }
}

