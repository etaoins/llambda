package llambda.frontend

import org.scalatest.{FunSuite,Inside}
import llambda._

class ExtractProgramSuite extends FunSuite with Inside {
  def programFor(scheme : String) = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(data, _) =>
        frontend.ExtractProgram(data)(testutil.TestLibraryLoader.apply)
      case err =>
        fail(err.toString)
    }
  }

  test("initial environment has no bindings") {
    intercept[UnboundVariableException] {
      programFor("(quote a)")
    }
  }
  
  test("import introduces bindings") {
    assert(programFor(
      """(import (test primitives)) 
         (quote a)"""
      ) === List(et.Literal(ast.Symbol("a"))))
  }
  
  test("multiple imports") {
    assert(programFor(
      """(import (only (test primitives) set!)) 
         (import (only (test primitives) lambda)) 
         set!
         lambda"""
      ) === List(et.VarReference(SchemePrimitives.Set), et.VarReference(SchemePrimitives.Lambda)))
  }

  test("program body is body context") {
    inside(programFor(
      """(import (test primitives))
         (define my-set set!)"""
    )) {
      case et.SetVar(_, expression) :: Nil =>
        assert(expression === et.VarReference(SchemePrimitives.Set))
    }
  }
}
