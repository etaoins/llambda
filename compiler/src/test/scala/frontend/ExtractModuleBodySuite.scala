package llambda.frontend

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class ExtractModuleBodySuite extends FunSuite with Inside with OptionValues with testutil.ExpressionHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(SchemePrimitives.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))
  
  test("variable reference") {
    // "a" isn't a binding in the primitive expressions
    intercept[UnboundVariableException] {
      expressionFor("a")
    }

    assert(expressionFor("+")(plusScope) === et.VarRef(plusLoc))
  }

  test("literals") {
    assert(expressionFor("'a") === et.Literal(ast.Symbol("a")))

    assert(expressionFor("'#(a b c)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c")))
    ))
    
    assert(expressionFor("'()") === et.Literal(ast.EmptyList()))
    
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
      ast.Bytevector(Vector(64, 65))
    ))
    
    assert(expressionFor("#u8(64 65)") === et.Literal(
      ast.Bytevector(Vector(64, 65))
    ))
    
    assert(expressionFor("'#t") === et.Literal(
      ast.BooleanLiteral(true)
    ))
    
    assert(expressionFor("#t") === et.Literal(
      ast.BooleanLiteral(true)
    ))
  }

  test("application") {
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

  test("primtivies cannot be expressions") {
    intercept[MalformedExpressionException] {
      expressionFor("include")
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
      et.Literal(ast.BooleanLiteral(true)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.Symbol("no"))
    ))
    
    assert(expressionFor("(if #f 'yes)") === et.Cond(
      et.Literal(ast.BooleanLiteral(false)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.UnspecificValue())
    ))
  }

  test("define variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)")(scope)

    // Make sure we preserved our source name for debugging purposes
    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.Bind(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))))
        ))

        assert(storageLoc.sourceName === "a")
    }
  }

  test("redefine variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define a 3)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.Bind(List(storageLoc -> et.Literal(ast.IntegerLiteral(2)))),
          et.MutateVar(storageLoc, et.Literal(ast.IntegerLiteral(3)))
        ))
    }
  }
  
  test("dependent define") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define b a)")(scope)

    inside((scope.get("a").value, scope.get("b").value)) {
      case (storageLocA : StorageLocation, storageLocB : StorageLocation) =>
        assert(expressions === List(
          et.Bind(List(storageLocA -> et.Literal(ast.IntegerLiteral(2)))),
          et.Bind(List(storageLocB -> et.VarRef(storageLocA)))
        ))
    }
  }
  
  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2) a")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.Bind(List(storageLoc -> et.Literal(ast.IntegerLiteral(2)))),
          et.VarRef(storageLoc)
        ))
    }
  }

  test("lambdas") {
    inside(expressionFor("(lambda () #t)")) {
      case et.Lambda(Nil, None, body) =>
        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(expressionFor("(lambda (x) x)")) {
      case et.Lambda(argX :: Nil, None, body) =>
        assert(body === et.VarRef(argX))
    }
    
    inside(expressionFor("(lambda x x)")) {
      case et.Lambda(Nil, Some(restArg), body) =>
        assert(body === et.VarRef(restArg))
    }

    inside(expressionFor("(lambda (x y . z) x y z)")) {
      case et.Lambda(argX :: argY :: Nil, Some(restArg), body) =>
        assert(body === et.Begin(List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        )))
    }
  }
  
  test("self-executing lambdas") {
    inside(expressionFor("((lambda (x) x) 1)")) {
      case et.Apply(et.Lambda(argX :: Nil, None, body), value :: Nil) =>
        assert(body === et.VarRef(argX))
        assert(value === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("recursive lambda define") {
    inside(expressionFor("""
      (lambda (x)
        (define foo (lambda (y) (bar x y)))
        (define bar (lambda (a b) (if a b)))
        (foo #t))""")) {
      case et.Lambda(xLoc :: Nil, None, et.Begin(bindExpr :: bodyExpr :: Nil)) =>
        inside(bindExpr) {
          case et.Bind(
            (fooLoc, et.Lambda(yLoc :: Nil, None, fooExpr)) ::
            (barLoc, et.Lambda(aLoc :: bLoc :: Nil, None, barExpr)) :: Nil) =>

          assert(fooExpr === et.Apply(et.VarRef(barLoc), et.VarRef(xLoc) :: et.VarRef(yLoc) :: Nil))
          assert(barExpr === et.Cond(et.VarRef(aLoc), et.VarRef(bLoc), et.Literal(ast.UnspecificValue())))

          assert(bodyExpr === et.Apply(et.VarRef(fooLoc), et.Literal(ast.BooleanLiteral(true)) :: Nil))
        }
    }
  }

  test("argless lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor("(define (return-true) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.Bind((procLoc, et.Lambda(Nil, None, bodyExpr)) :: Nil) =>
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
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
      case et.Bind((procLoc, et.Lambda(_ :: Nil, None, bodyExpr)) :: Nil) =>
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
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
      case et.Bind((procLoc, et.Lambda(_ :: Nil, Some(_), bodyExpr)) :: Nil) =>
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(false)))
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
      case et.Bind((procLoc, et.Lambda(Nil, Some(_), bodyExpr)) :: Nil) =>
        assert(bodyExpr === et.Literal(ast.IntegerLiteral(6)))
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
      case et.Bind((procLoc, et.Lambda(Nil, None, bodyExpr)) :: Nil) =>
        assert(bodyExpr === et.VarRef(procLoc))
    }
  }
  
  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      expressionFor("(lambda (x x) x)")
    }
  }

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor("(define x 1)(lambda (x) x)")(scope)
    inside(expressions) {
      case et.Bind((shadowed, _) :: Nil) :: et.Lambda(argX :: Nil, None, et.VarRef(inner)) :: Nil =>
        assert(inner != shadowed)
    }
  }
  
  test("define shadows") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor(
      """(define x 1)
         (lambda () (define x 2))"""
    )(scope) 

    inside(expressions) {
      case et.Bind((shadowed, _) :: Nil) :: et.Lambda(Nil, None, et.Bind((inner, _) :: Nil)) :: Nil =>
        assert(inner != shadowed)
    }
  }
  
  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define y 1)(lambda (x) y)")(scope)

    inside(expressions) {
      case et.Bind((outer, _) :: Nil) :: et.Lambda(argX :: Nil, None, et.VarRef(inner)) :: Nil =>
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
      case et.Bind((loc, et.Lambda(Nil, None, et.Literal(ast.BooleanLiteral(false)))) :: Nil) =>
        assert(loc === listBinding)
    }
  }

  test("trivial include") {
    // Simple include should return an et.Begin with the contents of the ifle
    assert(expressionFor("""(include "includes/include1.scm")""") ===
      et.Begin(List(
        et.Literal(ast.StringLiteral("include1-line1")), 
        et.Literal(ast.StringLiteral("include1-line2"))
      ))
    )
  }

  test("include multiple files in order") {
    // (include) with multiple files should read them in order and wrap them
    // in a single et.Begin
    assert(expressionFor("""(include "includes/include1.scm" "includes/include2.scm")""") ===
      et.Begin(List(
        et.Literal(ast.StringLiteral("include1-line1")), 
        et.Literal(ast.StringLiteral("include1-line2")),
        et.Literal(ast.StringLiteral("include2-line1")), 
        et.Literal(ast.StringLiteral("include2-line2"))
      ))
    )
  }

  test("include handles relative includes and scope correctly") {
    // This tests both relative (include)s and that scoping works correctly
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expression = expressionFor("""(include "includes/definea.scm")""")(scope)

    inside(scope("a")) {
      case storageLocA : StorageLocation =>
        inside(scope("b")) {
          case storageLocB : StorageLocation =>
            assert(expression === 
              et.Begin(List(
                et.Bind(List(storageLocA -> et.Literal(ast.IntegerLiteral(1)))),
                et.Begin(List(
                  et.Bind(List(storageLocB -> et.Literal(ast.IntegerLiteral(2)))),
                  et.Begin(List(
                    et.VarRef(storageLocA),
                    et.VarRef(storageLocB)
                  ))
                ))
              ))
            )
        }
    }
  }
}
