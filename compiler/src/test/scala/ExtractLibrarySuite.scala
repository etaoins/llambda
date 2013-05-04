package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class ExtractLibrarySuite extends FunSuite with Inside {
  val exampleName = List(StringComponent("example"), StringComponent("lib"))

  def libraryFor(scheme : String) = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(datum :: Nil, _) =>
        ExtractLibrary(datum)(util.TestLibraryLoader.apply)
      case err =>
        fail(err.toString)
    }
  }
  
  test("empty datum is invalid") {
    intercept[BadSpecialFormException] {
      libraryFor("()")
    }
  }

  test("empty library declaration") {
    assert(libraryFor(
      """(define-library (example lib))"""
      ) === Library(exampleName, Map(), Nil))
  }

  test("exporting imported symbol") {
    assert(libraryFor(
      """(define-library (example lib)
           (import (test primitives))
           (export set! lambda))"""
      ) === Library(exampleName, Map("set!" -> SchemePrimitives.Set, "lambda" -> SchemePrimitives.Lambda), Nil)) 
  }
  
  test("exporting internal symbol") {
    inside(libraryFor(
      """(define-library (example lib)
           (export number5)
           (begin 
             (define number5 5)))"""
     )) {
       case Library(_, bindings, exprs) =>
         assert(exprs === List(et.SetVar(bindings("number5"), et.Literal(ast.IntegerLiteral(5)))))
     }
  }

  test("exporting unbound idenitifer failure") {
    intercept[UnboundVariableException] {
      libraryFor(
        """(define-library (example lib)
             (export number5))""")
    }
  }
  
  test("renaming exports") {
    assert(libraryFor(
      """(define-library (example lib)
           (import (test primitives))
           (export set! (rename lambda new-lambda)))"""
      ) === Library(exampleName, Map("set!" -> SchemePrimitives.Set, "new-lambda" -> SchemePrimitives.Lambda), Nil)) 
  }
}
