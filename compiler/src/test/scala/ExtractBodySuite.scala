package test.scala

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class ExtractBodySuite extends FunSuite with Inside with OptionValues with util.ExpressionHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(SchemePrimitives.bindings.toSeq : _*))
  
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
      et.VarReference(plusLoc),
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
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(bodyFor("(define a 2)")(scope) === List(
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2)))
    ))
  }

  test("redefine variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(bodyFor("(define a 2)(define a 3)")(scope) === List(
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(3)))
    ))
  }
  
  test("indirect define") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(bodyFor("(define a 2)(define b a)")(scope) === List(
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
      et.SetVar(scope.get("b").value, et.VarReference(scope.get("a").value))
    ))
  }
  
  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(bodyFor("(define a 2) a")(scope) === List(
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2))),
      et.VarReference(scope.get("a").value)
    ))
  }

  test("lambdas") {
    inside(expressionFor("(lambda () #t)")) {
      case et.Procedure(Nil, None, body) =>
        assert(body === List(et.Literal(ast.TrueLiteral)))
    }

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
  
  test("self-executing lambdas") {
    inside(expressionFor("((lambda (x) x) 1)")) {
      case et.ProcedureCall(et.Procedure(argX :: Nil, None, body), value :: Nil) =>
        assert(body === List(et.VarReference(argX)))
        assert(value === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("argless lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor("(define (return-true) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(exprs) {
      case List(et.SetVar(procLoc, et.Procedure(Nil, None, bodyExprs))) =>
        assert(bodyExprs === List(et.Literal(ast.TrueLiteral)))
    }
  }
  
  test("one arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor("(define (return-true unused-param) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(exprs) {
      case List(et.SetVar(procLoc, et.Procedure(_ :: Nil, None, bodyExprs))) =>
        assert(bodyExprs === List(et.Literal(ast.TrueLiteral)))
    }
  }

  test("fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val exprs = bodyFor("(define (return-false some . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value
    inside(exprs) {
      case List(et.SetVar(procLoc, et.Procedure(_ :: Nil, Some(_), bodyExprs))) =>
        assert(bodyExprs === List(et.Literal(ast.FalseLiteral)))
    }
  }
    
  test("rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val exprs = bodyFor("(define (return-six . rest) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(exprs) {
      case List(et.SetVar(procLoc, et.Procedure(Nil, Some(_), bodyExprs))) =>
        assert(bodyExprs === List(et.Literal(ast.IntegerLiteral(6))))
    }
  }

  test("recursive lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor("(define (return-self) return-self)")(scope)
    val procLoc = scope.get("return-self").value
    inside(exprs) {
      case List(et.SetVar(procLoc, et.Procedure(Nil, None, bodyExprs))) =>
        assert(bodyExprs === List(et.VarReference(procLoc)))
    }
  }
  
  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      expressionFor("(lambda (x x) x)")
    }
  }

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor("(define x 1)(lambda (x) x)")(scope)
    inside(exprs) {
      case List(et.SetVar(shadowed, _), et.Procedure(argX :: Nil, None, et.VarReference(inner) :: Nil)) =>
        assert(inner != shadowed)
    }
  }
  
  test("define shadows") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor(
      """(define x 1)
         (lambda () (define x 2))"""
    )(scope) 

    inside(exprs) {
      case List(et.SetVar(shadowed, _), et.Procedure(Nil, None, et.SetVar(inner, _) :: Nil)) =>
        assert(inner != shadowed)
    }
  }
  
  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val exprs = bodyFor("(define y 1)(lambda (x) y)")(scope)

    inside(exprs) {
      case List(et.SetVar(outer, _), et.Procedure(argX :: Nil, None, et.VarReference(inner) :: Nil)) =>
        assert(outer === inner)
    }
  }

  test("define report procedure") {
    val allBindings = InternalPrimitives.bindings ++ SchemePrimitives.bindings
    val internalScope = new Scope(collection.mutable.Map(allBindings.toSeq : _*))

    val exprs = bodyFor("(define-report-procedure list (lambda () #f))")(internalScope)
    val listBinding = internalScope.get("list").value

    inside(listBinding) {
      case rp : ReportProcedure =>
        assert(rp.name === "list")
    }

    inside(exprs) {
      case et.SetVar(loc, et.Procedure(Nil, None, List(et.Literal(ast.FalseLiteral)))) :: Nil =>
        assert(loc === listBinding)
    }
  }
}
