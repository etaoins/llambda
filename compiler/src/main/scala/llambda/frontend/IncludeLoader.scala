package llambda.frontend

import llambda._
import java.net.URL
import java.io.FileNotFoundException
import annotation.tailrec

private[frontend] object IncludeLoader {
  private def attemptLoad(rootDir : URL, includeName : String)(implicit includePath : IncludePath) : Option[IncludeLoadResult] = {
    // Parse the include name relative to our root
    val includeUrl = new URL(rootDir, includeName)

    try {
      val stream = includeUrl.openStream()
    
      val libraryString = io.Source.fromInputStream(stream, "UTF-8").mkString

      // Find our filename
      val filename = includeUrl.getProtocol match {
        case "file" => includeUrl.getPath
        case _ => includeUrl.toString
      }
      
      val data = SchemeParser.parseStringAsData(libraryString, Some(filename))

      // Make a new IncludePath
      // This makes includes and libraries prefer loading other libraries from 
      // their own root directory and allows relative (include)s to work
      // correctly
      val innerIncludePath = includePath.copy(
        fileParentDir=Some(new URL(includeUrl, ".")),
        packageRootDir=Some(rootDir)
      )

      Some(IncludeLoadResult(
        innerIncludePath=innerIncludePath,
        data=data
      ))
    }
    catch {
      case _ : FileNotFoundException => None
    }
  }

  @tailrec
  def apply(rootDirs : Seq[URL], includeName : String)(implicit includePath : IncludePath) : Option[IncludeLoadResult] = rootDirs match {
    case headRootDir :: tailRootDirs =>
      attemptLoad(headRootDir, includeName) match {
        case Some(data) =>
          Some(data)
        case None =>
          apply(tailRootDirs, includeName)
      }

    case Nil =>
      None
  }
}
