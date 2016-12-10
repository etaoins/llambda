package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.platform
import llambda.compiler.{valuetype => vt}

case class GenGlobals(
  plannedSymbols : Set[String],
  generatedTypes : Map[vt.RecordLikeType, GeneratedType],
  constantGenerator : ConstantGenerator,
  targetPlatform : platform.TargetPlatform
)
