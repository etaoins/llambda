package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class CreateLambdaSuite extends FunSuite with Inside with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("lambdas") {
    inside(expressionFor("(lambda (x) x)")) {
      case et.Procedure(argX :: Nil, None, body) =>
        assert(body === List(et.VarReference(argX)))
    }
    
    inside(expressionFor("(lambda x x)")) {
      case et.Procedure(Nil, Some(restArg), body) =>
        assert(body === List(et.VarReference(restArg)))
    }

    inside(expressionFor("(lambda (x y . z) x y z)")) {
      case et.Procedure(argX :: argY :: Nil, Some(restArg), body) =>
        assert(body === List(
          et.VarReference(argX),
          et.VarReference(argY),
          et.VarReference(restArg)
        ))
    }
  }
}

