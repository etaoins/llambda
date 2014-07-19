package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._

class ExprLocatingSuite extends FunSuite with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  implicit val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

  test("variable references are located") {
    assertExprLocated(exprFor("+"))
  }
  
  test("applications are located") {
    assertExprLocated(exprFor("(+ 1 2)"))
  }

  test("literals are located") {
    assertExprLocated(exprFor("#t"))
  }

  test("conditions are located") {
    assertExprLocated(exprFor("(if #t 1 2)"))
  }

  test("non-trivial program is fully located") {
    val frontendConfig = frontend.FrontendConfig(
      includePath=testutil.NonTrivialProgram.includePath,
      featureIdentifiers=Set()
    )
    
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimizeLevel=0,
      targetPlatform=platform.Posix64LE,
      schemeDialect=dialect.Dialect.default
    )
  
    val loader = new frontend.LibraryLoader(compileConfig.targetPlatform)
    val expressions = frontend.ExtractProgram(None, testutil.NonTrivialProgram.data)(loader, frontendConfig)

    expressions.map(assertExprLocated)
  }
}

