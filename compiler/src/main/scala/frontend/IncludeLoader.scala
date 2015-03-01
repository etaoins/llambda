package io.llambda.compiler.frontend
import io.llambda

import scala.io.Source

import llambda.compiler._
import java.net.URL
import java.io.{File, FileNotFoundException}
import annotation.tailrec

import scala.collection.concurrent.TrieMap

private[frontend] object IncludeLoader {
  // This is a hack to prevent us from constantly re-parsing the same file during unit tests. This was especially
  // becoming a problem as (scheme base) grew in size. This ends up halving the time the unit tests take
  private val parsedCache = new TrieMap[String, Option[List[ast.Datum]]]

  private def attemptLoad(rootDir : URL, includeName : String) : Option[List[ast.Datum]] = {
    // Parse the include name relative to our root
    val includeUrl = new URL(rootDir, includeName)

    // Get our filename
    val includeFilename = includeUrl.getProtocol match {
      case "file" => includeUrl.getPath
      case _ => includeUrl.toString
    }

    parsedCache.getOrElseUpdate(includeFilename, {
      try {
        val libraryString = Source.fromFile(new File(includeFilename), "UTF-8").mkString

        // Success
        Some(SchemeParser.parseStringAsData(libraryString, Some(includeFilename)))
      }
      catch {
        case _ : FileNotFoundException => None
      }
    })
  }

  /** Attempts to load an include file of the specified name from a list of directories
    *
    * @param  rootDirs     List of directories to search for the include in search order
    * @param  includeName  Filename to search for
    * @return List of loaded data or None if the file could not be found
    */
  @tailrec
  def apply(rootDirs : Seq[URL], includeName : String) : Option[List[ast.Datum]] = rootDirs match {
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
