package io.llambda.compiler.frontend
import io.llambda

import java.net.URL

case class IncludePath(
  fileParentDir : Option[URL] = None,
  packageRootDir : Option[URL] = None,
  userConfiguredPaths : Seq[URL] = Nil
) {
  // Don't allow relative imports of libaries from the current file
  // Libraries are supposed to include their full path in their definition
  // Allowing file-relative imports would be confusing
  def librarySearchRoots : Seq[URL] = 
    (packageRootDir.toSeq ++ userConfiguredPaths ++ IncludePath.systemLibraryPaths).distinct

  // Only allow (include)s relative to the current file and the user configured  paths. To include from the program
  // root dir either relative paths or proper libraries can be used.
  // Don't allow includes from the system library path. Any include files are  intended for internal use only and will
  // be relative to the including file.
  def includeSearchRoots : Seq[URL] =
    (fileParentDir.toSeq ++ userConfiguredPaths).distinct
}

object IncludePath {
  val systemLibraryPaths = getClass.getClassLoader.getResource("libraries/") :: Nil
}
