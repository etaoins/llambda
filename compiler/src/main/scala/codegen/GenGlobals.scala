package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.platform

case class GenGlobals(
  plannedSymbols : Set[String],
  typeGenerator : TypeGenerator,
  debugInfoGeneratorOpt : Option[DebugInfoGenerator],
  targetPlatform : platform.TargetPlatform
)
