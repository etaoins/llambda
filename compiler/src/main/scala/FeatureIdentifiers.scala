package io.llambda.compiler


object FeatureIdentifiers {
  /** Feature identifier for the specific version of the compiler
    *
    * This is reused by codegen to identify ourselves in the final binary
    */
  val compilerVersionIdentifier = "llambda-0.3"

  private def commonFeatures = Set(
    // We only target LLVM at the moment
    "llvm",
    "llambda",
    compilerVersionIdentifier
  )

  def apply(extraIdents: Set[String] = Set()) =
    commonFeatures ++ extraIdents
}
