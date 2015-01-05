package io.llambda.compiler
import io.llambda

import llambda.compiler.frontend.IncludePath

case class CompileConfig(
  includePath : IncludePath,
  targetPlatform : platform.TargetPlatform,
  schemeDialect : dialect.Dialect,
  optimiseLevel : Int = 0,
  emitLlvm : Boolean = false,
  extraFeatureIdents : Set[String] = Set(),
  genDebugInfo : Boolean = false,
  saveTempObj : Boolean = false,
  dumpPlan : Boolean = false,
  traceMacroExpansion : Boolean = false
)
