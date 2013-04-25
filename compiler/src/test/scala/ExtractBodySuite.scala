package test.scala

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class ExtractBodySuite extends FunSuite with Inside with OptionValues with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("define variable") {
    inside(bodyFor("(define a 2)")) {
      case (exprs, scope) =>
        assert(exprs == List(
          et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2)))
        ))
    }
  }

  test("redefine variable") {
    inside(bodyFor("(define a 2)(define a 3)")) {
      case (exprs, scope) =>
        assert(exprs == List(
          et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
          et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(3)))
        ))
    }
  }
  
  test("indirect define") {
    inside(bodyFor("(define a 2)(define b a)")) {
      case (exprs, scope) =>
        assert(exprs == List(
          et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
          et.SetVar(scope.get("b").value, et.VarReference(scope.get("a").value))
        ))
    }
  }
  
  test("reference variable") {
    inside(bodyFor("(define a 2) a")) {
      case (exprs, scope) =>
        assert(exprs == List(
          et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
          et.VarReference(scope.get("a").value)
        ))
    }
  }
}
