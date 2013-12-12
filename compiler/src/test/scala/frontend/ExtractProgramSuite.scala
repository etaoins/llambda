package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler._

class ExtractProgramSuite extends FunSuite with Inside {
  def programFor(scheme : String) : List[et.Expression] = {
    val data = SchemeParser.parseStringAsData(scheme)
    frontend.ExtractProgram(data)(new LibraryLoader(platform.Posix64), IncludePath())
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
      case et.Bind((ref1, _) :: Nil) :: et.VarRef(ref2) :: et.VarRef(ref3) :: Nil =>
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
      case et.Bind((storageLoc1, _) :: Nil) :: et.Bind((storageLoc2, expression) :: Nil) :: Nil =>
        assert(expression === et.VarRef(storageLoc1))
    }
  }
}
