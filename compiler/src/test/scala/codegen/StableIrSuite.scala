package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite
import scala.io.Source

import llambda.compiler._

class StableIrSuite extends FunSuite {
  test("llvm IR is stable across compiles") {
    val lifeBaseDir = "life-example/"
    val lifeBaseUrl = getClass.getClassLoader.getResource(lifeBaseDir)
    val lifeProgramPath = s"${lifeBaseDir}life.scm"
  
    val includePath = frontend.IncludePath(
      fileParentDir=Some(lifeBaseUrl),
      packageRootDir=Some(lifeBaseUrl)
    )

    val stream = getClass.getClassLoader.getResourceAsStream(lifeProgramPath)

    if (stream == null) {
      throw new Exception(s"Unable to load Scheme test source from ${lifeProgramPath}")
    }

    val lifeProgramSource = Source.fromInputStream(stream, "UTF-8").mkString
    val parsed = SchemeParser.parseStringAsData(lifeProgramSource, Some(s":/${lifeProgramPath}"))
      
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimizeLevel=2,
      targetPlatform=platform.Posix64LE
    )

    // Compile the program 4 times
    val compiledVersions = (0 until 4).map { _ =>
      Compiler.compileDataToIr(parsed, compileConfig)
    }

    for(irPair <- compiledVersions.sliding(2)) {
      assert(irPair(0) === irPair(1))
    }
  }
}
