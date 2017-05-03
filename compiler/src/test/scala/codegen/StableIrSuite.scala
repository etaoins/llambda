package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._

class StableIrSuite extends FunSuite {
  test("llvm IR is stable across compiles") {
    val parsed = testutil.NonTrivialProgram.data

    val compileConfig = CompileConfig(
      includePath=testutil.NonTrivialProgram.includePath,
      optimiseLevel=2,
      targetPlatform=platform.ExamplePlatform.`x86_64-pc-linux-gnu`
    )

    // Compile the program 4 times
    val compiledVersions = (0 until 4).map { _ =>
      val plannedProgram = Compiler.planData(parsed, compileConfig)

      codegen.GenProgram(
        functions=plannedProgram.functions,
        compileConfig=compileConfig,
        entryFilenameOpt=None
      )
    }

    for(irPair <- compiledVersions.sliding(2)) {
      assert(irPair(0) === irPair(1))
    }
  }
}
