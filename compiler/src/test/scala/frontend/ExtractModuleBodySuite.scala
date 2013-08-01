package llambda.frontend

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class ExtractModuleBodySuite extends FunSuite with Inside with OptionValues with testutil.ExpressionHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(SchemePrimitives.bindings.toSeq : _*))
  
  test("variable reference") {
    // "a" isn't a binding in the primitive expressions
    intercept[UnboundVariableException] {
      expressionFor("a")
    }

    assert(expressionFor("set!") === et.VarRef(SchemePrimitives.Set))
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
    val plusLoc = new StorageLocation("+")
    val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

    assert(expressionFor("(+ 3 4)")(plusScope) === et.Apply(
      et.VarRef(plusLoc),
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
    val storageLoc = new StorageLocation("a")
    val varScope = new Scope(collection.mutable.Map("a" -> storageLoc), Some(primitiveScope))

    assert(expressionFor("(set! a 1)")(varScope) === et.MutateVar(
      storageLoc,
      et.Literal(ast.IntegerLiteral(1))
    ))

    intercept[UnboundVariableException] {
      expressionFor("(set! b 1)")
    }
    
    intercept[BadSpecialFormException] {
      expressionFor("(set! set! 1)")
    }
  }

  test("conditionals") {
    assert(expressionFor("(if #t 'yes 'no)") === et.Cond(
      et.Literal(ast.TrueLiteral),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.Symbol("no"))
    ))
    
    assert(expressionFor("(if #f 'yes)") === et.Cond(
      et.Literal(ast.FalseLiteral),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.UnspecificValue)
    ))
  }

  test("define variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)")(scope)

    // Make sure we preserved our source name for debugging purposes
    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.Let(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))), Nil)
        ))

        assert(storageLoc.sourceName === "a")
    }
  }

  test("redefine variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = expressionFor("(define a 2)(define a 3)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === 
          et.Let(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))),
            et.MutateVar(storageLoc, et.Literal(ast.IntegerLiteral(3))) :: Nil)
        )
    }
  }
  
  test("dependent define") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = expressionFor("(define a 2)(define b a)")(scope)

    inside((scope.get("a").value, scope.get("b").value)) {
      case (storageLocA : StorageLocation, storageLocB : StorageLocation) =>
        assert(expressions ===
          et.Let(List(storageLocA -> et.Literal(ast.IntegerLiteral(2))),
            et.Let(List(storageLocB -> et.VarRef(scope.get("a").value)), Nil) :: Nil)
        )
    }
  }
  
  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = expressionFor("(define a 2) a")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === 
          et.Let(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))),
            et.VarRef(storageLoc) :: Nil)
        )
    }
  }

  test("lambdas") {
    inside(expressionFor("(lambda () #t)")) {
      case et.Lambda(Nil, None, body) =>
        assert(body === List(et.Literal(ast.TrueLiteral)))
    }

    inside(expressionFor("(lambda (x) x)")) {
      case et.Lambda(argX :: Nil, None, body) =>
        assert(body === List(et.VarRef(argX)))
    }
    
    inside(expressionFor("(lambda x x)")) {
      case et.Lambda(Nil, Some(restArg), body) =>
        assert(body === List(et.VarRef(restArg)))
    }

    inside(expressionFor("(lambda (x y . z) x y z)")) {
      case et.Lambda(argX :: argY :: Nil, Some(restArg), body) =>
        assert(body === List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        ))
    }
  }
  
  test("self-executing lambdas") {
    inside(expressionFor("((lambda (x) x) 1)")) {
      case et.Apply(et.Lambda(argX :: Nil, None, body), value :: Nil) =>
        assert(body === List(et.VarRef(argX)))
        assert(value === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("recursive lambda define") {
    inside(expressionFor("""
      (lambda (x)
        (define foo (lambda (y) (bar x y)))
        (define bar (lambda (a b) (if a b)))
        (foo #t))""")) {
      case et.Lambda(xLoc :: Nil, None, outerExpr :: Nil) =>
        inside(outerExpr) {
          case et.Let(
            (fooLoc, et.Lambda(yLoc :: Nil, None, fooExpr :: Nil)) ::
            (barLoc, et.Lambda(aLoc :: bLoc :: Nil, None, barExpr :: Nil)) :: Nil,
            outerBodyExpr :: Nil) =>

          assert(fooExpr === et.Apply(et.VarRef(barLoc), et.VarRef(xLoc) :: et.VarRef(yLoc) :: Nil))
          assert(barExpr === et.Cond(et.VarRef(aLoc), et.VarRef(bLoc), et.Literal(ast.UnspecificValue)))
          assert(outerBodyExpr === et.Apply(et.VarRef(fooLoc), et.Literal(ast.TrueLiteral) :: Nil))
        }
    }
  }

  test("argless lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor("(define (return-true) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.Let((procLoc, et.Lambda(Nil, None, bodyExprs)) :: Nil, Nil) =>
        assert(bodyExprs === List(et.Literal(ast.TrueLiteral)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }
  
  test("one arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor("(define (return-true unused-param) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.Let((procLoc, et.Lambda(_ :: Nil, None, bodyExprs)) :: Nil, Nil) =>
        assert(bodyExprs === List(et.Literal(ast.TrueLiteral)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }

  test("fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val expr = expressionFor("(define (return-false some . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.Let((procLoc, et.Lambda(_ :: Nil, Some(_), bodyExprs)) :: Nil, Nil) =>
        assert(bodyExprs === List(et.Literal(ast.FalseLiteral)))
    }

    inside(scope.get("return-false").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-false")
    }
  }
    
  test("rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val expr = expressionFor("(define (return-six . rest) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(expr) {
      case et.Let((procLoc, et.Lambda(Nil, Some(_), bodyExprs)) :: Nil, Nil) =>
        assert(bodyExprs === List(et.Literal(ast.IntegerLiteral(6))))
    }
    
    inside(scope.get("return-six").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-six")
    }
  }

  test("recursive lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor("(define (return-self) return-self)")(scope)
    val procLoc = scope.get("return-self").value
    inside(expr) {
      case et.Let((procLoc, et.Lambda(Nil, None, bodyExprs)) :: Nil, Nil) =>
        assert(bodyExprs === List(et.VarRef(procLoc)))
    }
  }
  
  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      expressionFor("(lambda (x x) x)")
    }
  }

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor("(define x 1)(lambda (x) x)")(scope)
    inside(expr) {
      case et.Let((shadowed, _) :: Nil, et.Lambda(argX :: Nil, None, et.VarRef(inner) :: Nil) :: Nil) =>
        assert(inner != shadowed)
    }
  }
  
  test("define shadows") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = expressionFor(
      """(define x 1)
         (lambda () (define x 2))"""
    )(scope) 

    inside(exprs) {
      case et.Let((shadowed, _) :: Nil, et.Lambda(Nil, None, et.Let((inner, _) :: Nil, Nil) :: Nil) :: Nil) =>
        assert(inner != shadowed)
    }
  }
  
  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expr = expressionFor("(define y 1)(lambda (x) y)")(scope)

    inside(expr) {
      case et.Let((outer, _) :: Nil, et.Lambda(argX :: Nil, None, et.VarRef(inner) :: Nil) :: Nil) =>
        assert(outer === inner)
    }
  }

  test("define report procedure") {
    val allBindings = InternalPrimitives.bindings ++ SchemePrimitives.bindings
    val internalScope = new Scope(collection.mutable.Map(allBindings.toSeq : _*))

    val expr = expressionFor("(define-report-procedure list (lambda () #f))")(internalScope)
    val listBinding = internalScope.get("list").value

    inside(listBinding) {
      case rp : ReportProcedure =>
        assert(rp.reportName === "list")
    }

    inside(expr) {
      case et.Let((loc, et.Lambda(Nil, None, List(et.Literal(ast.FalseLiteral)))) :: Nil, Nil) =>
        assert(loc === listBinding)
    }
  }
}
