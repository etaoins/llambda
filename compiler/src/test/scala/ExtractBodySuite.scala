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


  test("lambda shorthand") {
    inside(bodyFor("(define (return-true unused-param) #t)")) {
      case (exprs, scope) =>
        val procLoc = scope.get("return-true").value
        inside(exprs) {
          case List(et.SetVar(procLoc, et.Procedure(_ :: Nil, None, bodyExprs))) =>
            assert(bodyExprs === List(et.Literal(ast.TrueLiteral)))
        }
    }
    
    inside(bodyFor("(define (return-false some . rest) #f)")) {
      case (exprs, scope) =>
        val procLoc = scope.get("return-false").value
        inside(exprs) {
          case List(et.SetVar(procLoc, et.Procedure(_ :: Nil, Some(_), bodyExprs))) =>
            assert(bodyExprs === List(et.Literal(ast.FalseLiteral)))
        }
    }
    
    inside(bodyFor("(define (return-six . rest) 6)")) {
      case (exprs, scope) =>
        val procLoc = scope.get("return-six").value
        inside(exprs) {
          case List(et.SetVar(procLoc, et.Procedure(Nil, Some(_), bodyExprs))) =>
            assert(bodyExprs === List(et.Literal(ast.IntegerLiteral(6))))
        }
    }
  }
}
