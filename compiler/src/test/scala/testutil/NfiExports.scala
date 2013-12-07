package llambda.testutil

import llambda.frontend.{LibraryLoader, IncludePath}
import llambda._

object NfiExports {
  def apply() : collection.mutable.Map[String, BoundValue] = {
    implicit val includePath = IncludePath()
    val libraryLoader = new LibraryLoader(platform.Posix64)

    val exports = libraryLoader.load(List(StringComponent("llambda"), StringComponent("nfi")))

    collection.mutable.Map(exports.toSeq : _*)
  }
}

