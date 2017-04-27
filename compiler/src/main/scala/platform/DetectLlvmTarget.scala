package io.llambda.compiler.platform


object DetectLlvmTarget {
  def fromLlvmIr(llvmir: String): Option[TargetPlatform] = {
    //val TripleString = """(?m)^target triple\s*=\s*"([^"]*)"\s*$""".r
    val TripleString = """(?m)^\s*target\s+triple\s*=\s*"([^"]*)"\s*$""".r

    TripleString.findFirstMatchIn(llvmir) flatMap { m =>
      fromTargetTriple(m.group(1))
    }
  }

  def fromTargetTriple(triple: String): Option[TargetPlatform] = {
    val Arm32ArchPattern = "^arm.*".r
    val Ia32ArchPattern = "^i[3-9]86$".r

    val tripleParts = triple.split("-").toList

    val isWindows = tripleParts match {
      case _ :: _ :: "windows" :: _ => true
      case _ => false
    }

    triple.split("-").toList match {
      case "x86_64" :: _ =>
        if (isWindows) {
          Some(Win64)
        }
        else {
          Some(Posix64LE)
        }

      case Ia32ArchPattern() :: _ | Arm32ArchPattern() :: _ =>
        if (isWindows) {
          Some(Win32)
        }
        else {
          Some(Posix32LE)
        }

      case _ => None
    }
  }
}
