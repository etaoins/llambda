package io.llambda.compiler

import io.llambda
import platform.TargetPlatform

object FeatureIdentifiers {
  /** Feature identifier for the specific version of the compiler
    *
    * This is reused by codegen to identify ourselves in the final binary
    */
  val compilerVersionIdentifier = "llambda-0.0"

  private def commonFeatures = Set(
    // We only target LLVM at the moment
    "llvm",

    // XXX: It would be nice to run clang++ or llc to extract more CPU-specific feature identifiers from the target
    // triple. It might not be worth the overhead without a specific use-case, however.

    "llambda",
    compilerVersionIdentifier
  )

  def apply(platform : TargetPlatform, extraIdents : Set[String] = Set()) =
    commonFeatures ++ platform.platformFeatures ++ extraIdents
}
