package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class ScopeSuite extends FunSuite with Inside with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("shadowing") {
    inside(bodyFor("(define x 1)(lambda (x) x)")) {
      case (exprs, scope) =>
        inside(exprs) {
          case List(et.SetVar(shadowed, _), et.Procedure(argX :: Nil, None, et.VarReference(inner) :: Nil)) =>
            assert(inner != shadowed)
        }
    }
  }
  
  test("capturing") {
    inside(bodyFor("(define y 1)(lambda (x) y)")) {
      case (exprs, scope) =>
        inside(exprs) {
          case List(et.SetVar(outer, _), et.Procedure(argX :: Nil, None, et.VarReference(inner) :: Nil)) =>
            assert(outer === inner)
        }
    }
  }
}


