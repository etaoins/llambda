package test.scala

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class ExtractBodySuite extends FunSuite with Inside with OptionValues with util.ExpressionHelpers {
  implicit val primitiveScope = new Scope(collection.mutable.Map(SchemePrimitives.bindings.toSeq : _*))
  
  test("variable reference") {
    // "a" isn't a binding in the primitive expressions
    intercept[UnboundVariableException] {
      expressionFor("a")
    }

    assert(expressionFor("set!") === et.VarReference(SchemePrimitives.Set))
  }

  test("literals") {
    assert(expressionFor("'a") === et.Literal(ast.Symbol("a")))

    assert(expressionFor("'#(a b c)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c")))
    ))
    
    assert(expressionFor("'()") === et.Literal(ast.EmptyList))
    
    assert(expressionFor("'(+ 1 2)") === et.Literal(
      ast.ProperList(List(ast.Symbol("+"), ast.IntegerLiteral(1), ast.IntegerLiteral(2)))
    ))
    
    assert(expressionFor("'(quote a)") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))
    
    assert(expressionFor("''a") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))

    assert(expressionFor("'145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(expressionFor("145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(expressionFor("'\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(expressionFor("\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(expressionFor("'#(a 10)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.IntegerLiteral(10)))
    ))
    
    assert(expressionFor("#(a 10)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.IntegerLiteral(10)))
    ))
    
    assert(expressionFor("'#u8(64 65)") === et.Literal(
      ast.ByteVector(Vector(64, 65))
    ))
    
    assert(expressionFor("#u8(64 65)") === et.Literal(
      ast.ByteVector(Vector(64, 65))
    ))
    
    assert(expressionFor("'#t") === et.Literal(
      ast.TrueLiteral
    ))
    
    assert(expressionFor("#t") === et.Literal(
      ast.TrueLiteral
    ))
  }

  test("application") {
    // Make a storage location for +
    val plusLoc = new StorageLocation
    val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

    assert(expressionFor("(+ 3 4)")(plusScope) === et.ProcedureCall(
      plusLoc,
      List(
        et.Literal(ast.IntegerLiteral(3)),
        et.Literal(ast.IntegerLiteral(4))
      )
    ))

    intercept[UnboundVariableException] {
      expressionFor("(/ 1 2)")
    }

    // R7RS explicitly calls this an error
    intercept[MalformedExpressionException] {
      expressionFor("()")
    }
  }

  test("set!") {
    // Make a storage location for a
    val storageLoc = new StorageLocation
    val varScope = new Scope(collection.mutable.Map("a" -> storageLoc), Some(primitiveScope))

    assert(expressionFor("(set! a 1)")(varScope) === et.SetVar(
      storageLoc,
      et.Literal(ast.IntegerLiteral(1))
    ))

    intercept[UnboundVariableException] {
      expressionFor("(set! b 1)")
    }
  }

  test("conditionals") {
    assert(expressionFor("(if #t 'yes 'no)") === et.Conditional(
      et.Literal(ast.TrueLiteral),
      et.Literal(ast.Symbol("yes")),
      Some(et.Literal(ast.Symbol("no")))
    ))
    
    assert(expressionFor("(if #f 'yes)") === et.Conditional(
      et.Literal(ast.FalseLiteral),
      et.Literal(ast.Symbol("yes")),
      None
    ))
  }

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
