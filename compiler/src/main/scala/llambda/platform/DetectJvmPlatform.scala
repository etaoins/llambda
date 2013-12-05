package llambda.platform

class JvmDetectFailedException extends Exception("Unable to classify the JVM architecture") 

object DetectJvmPlatform {
  private lazy val detectedPlatform = {
    val dataModelBits = System.getProperty("sun.arch.data.model").toInt
    val isWindows = util.Properties.isWin

    (dataModelBits, isWindows) match {
      case (64, false) => Posix64
      case (32, false) => Posix32
      case (64, true)  => Win64
      case (32, true)  => Win32

      case _ => throw new JvmDetectFailedException
    }
  }

  def apply() : TargetPlatform = detectedPlatform
}

