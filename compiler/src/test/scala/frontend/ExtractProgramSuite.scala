package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler._

class ExtractProgramSuite extends FunSuite with Inside {
  def programFor(scheme : String) : List[et.Expr] = {
    val data = SchemeParser.parseStringAsData(scheme)
    val frontendConfig = FrontendConfig(
      includePath=IncludePath(Nil),
      featureIdentifiers=Set()
    )

    frontend.ExtractProgram(data)(new LibraryLoader(platform.Posix64LE), frontendConfig)
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
      case List(et.TopLevelDefine(et.Binding(ref1, _)), et.VarRef(ref2), et.VarRef(ref3)) =>
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
      case List(
           et.TopLevelDefine(et.Binding(storageLoc1, _)),
           et.TopLevelDefine(et.Binding(storageLoc2, expression))
      ) =>
        assert(expression === et.VarRef(storageLoc1))
    }
  }
}
