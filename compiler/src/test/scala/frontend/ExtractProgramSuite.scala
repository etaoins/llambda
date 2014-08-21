package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler._

class ExtractProgramSuite extends FunSuite with Inside {
  def programFor(scheme : String) : List[et.Expr] = {
    val data = SchemeParser.parseStringAsData(scheme)
    val frontendConfig = FrontendConfig(
      includePath=IncludePath(),
      featureIdentifiers=Set(),
      schemeDialect=dialect.Dialect.default
    )

    frontend.ExtractProgram(None, data)(new LibraryLoader(platform.Posix64LE), frontendConfig)
  }

  test("initial environment has no bindings") {
    intercept[UnboundVariableException] {
      programFor("(quote a)")
    }
  }
  
  test("import introduces bindings") {
    assert(programFor(
      """(import (llambda internal primitives)) 
         (quote a)"""
      ) === List(et.Literal(ast.Symbol("a"))))
  }
  
  test("multiple imports") {
    inside(programFor(
      """(import (only (test singleexpr) a)) 
         (import (rename (test singleexpr) (a b))) 
         a
         b"""
    )) {
      case et.TopLevelDefine((ref1, _) :: Nil) :: et.VarRef(ref2) :: et.VarRef(ref3) :: Nil =>
        assert(ref1 === ref2)
        assert(ref2 === ref3)
    }
  }

  test("program body is body context") {
    inside(programFor(
      """(import (test singleexpr))
         (import (llambda internal primitives))
         (define b a)"""
    )) {
      case et.TopLevelDefine((storageLoc1, _) :: Nil) :: et.TopLevelDefine((storageLoc2, expression) :: Nil) :: Nil =>
        assert(expression === et.VarRef(storageLoc1))
    }
  }
}
