package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class ExtractBodySuite extends FunSuite with Inside with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("define variable") {
    inside(bodyFor("(define a 2)")) {
      case (exprs, scope) =>
        inside(scope.get("a")) {
          case Some(locA) =>
            assert(exprs == List(
              et.SetVar(locA, et.Literal(ast.IntegerLiteral(2)))
            ))
        }
    }
  }

  test("redefine variable") {
    inside(bodyFor("(define a 2)(define a 3)")) {
      case (exprs, scope) =>
        inside(scope.get("a")) {
          case Some(locA) =>
            assert(exprs == List(
              et.SetVar(locA, et.Literal(ast.IntegerLiteral(2))),
              et.SetVar(locA, et.Literal(ast.IntegerLiteral(3)))
            ))
        }
    }
  }
  
  test("indirect define") {
    inside(bodyFor("(define a 2)(define b a)")) {
      case (exprs, scope) =>
        inside((scope.get("a"), scope.get("b"))) {
          case (Some(locA), Some(locB)) =>
            assert(exprs == List(
              et.SetVar(locA, et.Literal(ast.IntegerLiteral(2))),
              et.SetVar(locB, et.VarReference(locA))
            ))
        }
    }
  }
  
  test("reference variable") {
    inside(bodyFor("(define a 2) a")) {
      case (exprs, scope) =>
        inside(scope.get("a")) {
          case Some(locA) =>
            assert(exprs == List(
              et.SetVar(locA, et.Literal(ast.IntegerLiteral(2))),
              et.VarReference(locA)
            ))
        }
    }
  }
}
