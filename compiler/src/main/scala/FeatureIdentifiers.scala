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
    // From R7RS errata: "The algebraic operations +, -, *, and expt where the second argument is a non-negative
    // integer) all produce exact values given exact inputs". This is true because we will either calculate the result
    // as an integer or raise an integer overflow
    "exact-closed",

    // We don't support any complex numbers
    // "exact-complex"

    // LLVM gives us IEEE floats
    "ieee-float",

    // Our runtime has full Unicode 6.0 support
    "full-unicode",

    // We have no ratio support
    // "ratios`",

    // We only target LLVM at the moment
    "llvm",

    // XXX: It would be nice to run clang++ or llc to extract more CPU-specific feature identifiers from the target
    // triple. It might not be worth the overhead without a specific use-case, however.

    "llambda",
    compilerVersionIdentifier
  )

  def apply(platform : TargetPlatform, schemeDialect : dialect.Dialect, extraIdents : Set[String] = Set()) =
    commonFeatures ++ platform.platformFeatures ++ schemeDialect.dialectFeatures ++ extraIdents
}
