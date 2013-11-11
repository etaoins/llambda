package llambda.frontend

import org.scalatest.{FunSuite,Inside}
import llambda._

class ExtractProgramSuite extends FunSuite with Inside {
  def programFor(scheme : String) : List[et.Expression] = {
    val data = SchemeParser.parseStringAsData(scheme)
    frontend.ExtractProgram(data)(new LibraryLoader, IncludePath())
  }

  test("initial environment has no bindings") {
    intercept[UnboundVariableException] {
      programFor("(quote a)")
    }
  }
  
  test("import introduces bindings") {
    assert(programFor(
      """(import (llambda primitives)) 
         (quote a)"""
      ) === List(et.Literal(ast.Symbol("a"))))
  }
  
  test("multiple imports") {
    assert(programFor(
      """(import (only (llambda primitives) set!)) 
         (import (only (llambda primitives) lambda)) 
         set!
         lambda"""
      ) === List(et.VarRef(SchemePrimitives.Set), et.VarRef(SchemePrimitives.Lambda)))
  }

  test("program body is body context") {
    inside(programFor(
      """(import (llambda primitives))
         (define my-set set!)"""
    )) {
      case et.Bind((_, expression) :: Nil) :: Nil =>
        assert(expression === et.VarRef(SchemePrimitives.Set))
    }
  }
}
