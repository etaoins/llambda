package io.llambda.compiler.frontend
import io.llambda

import java.net.URL

/** Represents a list of paths to search for libraries and included files */
case class IncludePath(userConfiguredPaths: Seq[URL]) {
  /** Returns a list of paths to search for libraries */
  def librarySearchRoots: Seq[URL] =
    (userConfiguredPaths ++ IncludePath.systemLibraryPaths).distinct

  /** Returns a list of paths for search for includes
    *
    * This intentionally doesn't include library paths. Libraries should only expose library definitions and should be
    * free to use internal relative includes without polluting the global include namespace.
    */
  def includeSearchRoots: Seq[URL] =
    userConfiguredPaths
}

object IncludePath {
  /** List of system library search paths
    *
    * These are searched for libraries after all user configured paths
    */
  val systemLibraryPaths = List(getClass.getClassLoader.getResource("libraries/"))
}
