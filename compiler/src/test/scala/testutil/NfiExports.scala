package io.llambda.compiler.testutil
import io.llambda

import llambda.compiler.frontend.{LibraryLoader, IncludePath}
import llambda.compiler._

object NfiExports {
  def apply() : collection.mutable.Map[String, BoundValue] = {
    val libraryLoader = new LibraryLoader(platform.Posix64LE)
    implicit val frontendConfig = frontend.FrontendConfig(
      includePath=IncludePath(),
      featureIdentifiers=Set()
    )

    val exports = libraryLoader.load(List(StringComponent("llambda"), StringComponent("nfi")))

    collection.mutable.Map(exports.toSeq : _*)
  }
}

