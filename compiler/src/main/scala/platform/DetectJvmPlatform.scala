package io.llambda.compiler.platform
import io.llambda

import java.nio.ByteOrder
import java.nio.ByteOrder.{BIG_ENDIAN, LITTLE_ENDIAN}

class JvmDetectFailedException extends Exception("Unable to classify the JVM architecture") 

object DetectJvmPlatform {
  private lazy val detectedPlatform = {
    val dataModelBits = System.getProperty("sun.arch.data.model").toInt
    val isWindows = util.Properties.isWin

    (dataModelBits, isWindows, ByteOrder.nativeOrder) match {
      case (64, false, BIG_ENDIAN)    => Posix64BE
      case (64, false, LITTLE_ENDIAN) => Posix64LE
      case (32, false, BIG_ENDIAN)    => Posix32BE
      case (32, false, LITTLE_ENDIAN) => Posix32LE

      // Consumer Windows only comes in little-endian
      // Xbox 360 is big-endian but it's doubtful a public LLVM release will
      // ever generate code for it
      case (64, true, LITTLE_ENDIAN)  => Win64
      case (32, true, LITTLE_ENDIAN)  => Win32

      case _ => throw new JvmDetectFailedException
    }
  }

  def apply() : TargetPlatform = detectedPlatform
}

