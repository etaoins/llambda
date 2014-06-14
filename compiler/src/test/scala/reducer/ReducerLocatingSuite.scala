package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler._
import org.scalatest.FunSuite

class ReducerLocatingSuite extends FunSuite with testutil.ExprHelpers {
  test("non-trivial program is fully located after reduction") {
    val frontendConfig = frontend.FrontendConfig(
      includePath=testutil.NonTrivialProgram.includePath,
      featureIdentifiers=Set()
    )
    
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimizeLevel=0,
      targetPlatform=platform.DetectJvmPlatform()
    )
  
    val loader = new frontend.LibraryLoader(compileConfig.targetPlatform)
    val expressions = frontend.ExtractProgram(None, testutil.NonTrivialProgram.data)(loader, frontendConfig)

    val analysis = reducer.AnalyseExprs(expressions)
    assertExprLocated(reducer.ReduceExprs(analysis))
  }
}
