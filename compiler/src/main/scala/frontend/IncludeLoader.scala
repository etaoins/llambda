package io.llambda.compiler.frontend
import io.llambda

import scala.io.Source

import llambda.compiler._
import java.net.URL
import java.io.FileNotFoundException
import annotation.tailrec

import scala.collection.mutable.HashMap

private[frontend] object IncludeLoader {
  // This is a hack to prevent us from constantly re-parsing the same file during unit tests. This was especially
  // becoming a problem as (scheme base) grew in size. This ends up halving the time the unit tests took

  val parsedCache = new HashMap[URL, Option[List[ast.Datum]]]

  // Some() indicates the file was found while None indicates it wasn't
  private def cachedLoadAndParse(includeUrl : URL) : Option[List[ast.Datum]] = 
    parsedCache.synchronized {
      parsedCache.getOrElseUpdate(includeUrl, {
        try {
          val stream = includeUrl.openStream()
        
          val libraryString = Source.fromInputStream(stream, "UTF-8").mkString

          // Find our filename
          val filename = includeUrl.getProtocol match {
            case "file" => includeUrl.getPath
            case _ => includeUrl.toString
          }
        
          // Success
          Some(SchemeParser.parseStringAsData(libraryString, Some(filename)))
        }
        catch {
          case _ : FileNotFoundException => None
        }
    })
  }

  private def attemptLoad(rootDir : URL, includeName : String)(implicit includePath : IncludePath) : Option[IncludeLoadResult] = {
    // Parse the include name relative to our root
    val includeUrl = new URL(rootDir, includeName)

    cachedLoadAndParse(includeUrl) map { data =>
      // Make a new IncludePath
      // This makes includes and libraries prefer loading other libraries from 
      // their own root directory and allows relative (include)s to work
      // correctly
      val innerIncludePath = includePath.copy(
        fileParentDir=Some(new URL(includeUrl, ".")),
        packageRootDir=Some(rootDir)
      )

      IncludeLoadResult(
        innerIncludePath=innerIncludePath,
        data=data
      )
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
