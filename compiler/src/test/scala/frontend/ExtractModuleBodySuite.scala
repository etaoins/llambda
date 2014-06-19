package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

class ExtractModuleBodySuite extends FunSuite with Inside with OptionValues with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(PrimitiveExprs.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))
  
  test("variable reference") {
    // "a" isn't a binding in the primitive expressions
    intercept[UnboundVariableException] {
      exprFor("a")
    }

    assert(exprFor("+")(plusScope) === et.VarRef(plusLoc))
  }

  test("literals") {
    assert(exprFor("'a") === et.Literal(ast.Symbol("a")))

    assert(exprFor("'#(a b c)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c")))
    ))
    
    assert(exprFor("'()") === et.Literal(ast.EmptyList()))
    
    assert(exprFor("'(+ 1 2)") === et.Literal(
      ast.ProperList(List(ast.Symbol("+"), ast.IntegerLiteral(1), ast.IntegerLiteral(2)))
    ))
    
    assert(exprFor("'(quote a)") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))
    
    assert(exprFor("''a") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))

    assert(exprFor("'145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(exprFor("145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(exprFor("'\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(exprFor("\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(exprFor("'#(a 10)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.IntegerLiteral(10)))
    ))
    
    assert(exprFor("#(a 10)") === et.Literal(
      ast.VectorLiteral(Vector(ast.Symbol("a"), ast.IntegerLiteral(10)))
    ))
    
    assert(exprFor("'#u8(64 65)") === et.Literal(
      ast.Bytevector(Vector(64, 65))
    ))
    
    assert(exprFor("#u8(64 65)") === et.Literal(
      ast.Bytevector(Vector(64, 65))
    ))
    
    assert(exprFor("'#t") === et.Literal(
      ast.BooleanLiteral(true)
    ))
    
    assert(exprFor("#t") === et.Literal(
      ast.BooleanLiteral(true)
    ))
    
    assert(exprFor("#!unit") === et.Literal(
      ast.UnitValue()
    ))
  }

  test("application") {
    assert(exprFor("(+ 3 4)")(plusScope) === et.Apply(
      et.VarRef(plusLoc),
      List(
        et.Literal(ast.IntegerLiteral(3)),
        et.Literal(ast.IntegerLiteral(4))
      )
    ))

    intercept[UnboundVariableException] {
      exprFor("(/ 1 2)")
    }

    // R7RS explicitly calls this an error
    intercept[MalformedExprException] {
      exprFor("()")
    }
  }

  test("primtivies cannot be expressions") {
    intercept[MalformedExprException] {
      exprFor("include")
    }
  }

  test("set!") {
    // Make a storage location for a
    val storageLoc = new StorageLocation("a")
    val varScope = new Scope(collection.mutable.Map("a" -> storageLoc), Some(primitiveScope))

    assert(exprFor("(set! a 1)")(varScope) === et.MutateVar(
      storageLoc,
      et.Literal(ast.IntegerLiteral(1))
    ))

    intercept[UnboundVariableException] {
      exprFor("(set! b 1)")
    }
    
    intercept[BadSpecialFormException] {
      exprFor("(set! set! 1)")
    }
  }

  test("conditionals") {
    assert(exprFor("(if #t 'yes 'no)") === et.Cond(
      et.Literal(ast.BooleanLiteral(true)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.Symbol("no"))
    ))
    
    assert(exprFor("(if #f 'yes)") === et.Cond(
      et.Literal(ast.BooleanLiteral(false)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.UnitValue())
    ))
  }

  test("define untyped variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(!storageLoc.hasTypeConstraints)
    }
  }
  
  test("define typed variable") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("(define: a : <exact-integer-cell> 2)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(storageLoc -> et.Literal(ast.IntegerLiteral(2))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(storageLoc.schemeType === vt.IntrinsicCellType(ct.ExactIntegerCell))
    }
  }

  test("redefine variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define a 3)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(storageLoc -> et.Literal(ast.IntegerLiteral(2)))),
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
          et.TopLevelDefine(List(storageLocA -> et.Literal(ast.IntegerLiteral(2)))),
          et.TopLevelDefine(List(storageLocB -> et.VarRef(storageLocA)))
        ))
    }
  }
  
  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2) a")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(storageLoc -> et.Literal(ast.IntegerLiteral(2)))),
          et.VarRef(storageLoc)
        ))
    }
  }
  
  test("define type") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("(define-type <custom-type> <int32>)")(scope)

    assert(scope("<custom-type>") === BoundType(vt.Int32))

    intercept[UnboundVariableException] {
      bodyFor("(define-type <another-type> <doesnt-exist>)")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Not enough args
      bodyFor("(define-type <another-type>)")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Too many args
      bodyFor("(define-type <another-type> <int32> <unicode-char>)")(scope)
    }
  }

  test("untyped lambdas") {
    inside(exprFor("(lambda () #t)")) {
      case et.Lambda(Nil, None, body, _) =>
        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(exprFor("(lambda (x) x)")) {
      case et.Lambda(List(argX), None, body, _) =>
        assert(argX.hasTypeConstraints === false)
        assert(body === et.VarRef(argX))
    }
    
    inside(exprFor("(lambda x x)")) {
      case et.Lambda(Nil, Some(restArg), body, _) =>
        assert(restArg.hasTypeConstraints === false)
        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda (x y . z) x y z)")) {
      case et.Lambda(List(argX, argY), Some(restArg), body, _) =>
        assert(argX.hasTypeConstraints === false)
        assert(argY.hasTypeConstraints === false)
        assert(restArg.hasTypeConstraints === false)

        assert(body === et.Begin(List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        )))
    }
  }
  
  test("typed lambdas") {
    inside(exprFor("(lambda: () #t)")(nfiScope)) {
      case et.Lambda(Nil, None, body, _) =>
        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(exprFor("(lambda: ((x : <numeric-cell>)) x)")(nfiScope)) {
      case et.Lambda(List(argX), None, body, _) =>
        assert(argX.schemeType === vt.IntrinsicCellType(ct.NumericCell))
        assert(body === et.VarRef(argX))
    }
    
    inside(exprFor("(lambda: x x)")(nfiScope)) {
      case et.Lambda(Nil, Some(restArg), body, _) =>
        assert(restArg.hasTypeConstraints === false)
        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda: ((x : <exact-integer-cell>) (y : <string-cell>) . z) x y z)")(nfiScope)) {
      case et.Lambda(List(argX, argY), Some(restArg), body, _) =>
        assert(argX.schemeType === vt.IntrinsicCellType(ct.ExactIntegerCell))
        assert(argY.schemeType === vt.IntrinsicCellType(ct.StringCell))
        assert(restArg.hasTypeConstraints === false)

        assert(body === et.Begin(List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        )))
    }
  }
  
  test("self-executing lambdas") {
    inside(exprFor("((lambda (x) x) 1)")) {
      case et.Apply(et.Lambda(List(argX), None, body, _), List(value)) =>
        assert(body === et.VarRef(argX))
        assert(value === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("recursive lambda define") {
    inside(exprFor("""
      (lambda (x)
        (define foo (lambda (y) (bar x y)))
        (define bar (lambda (a b) (if a b)))
        (foo #t))""")) {
      case et.Lambda(List(xLoc), None, et.InternalDefine(
        List(
          (fooLoc, et.Lambda(List(yLoc), None, fooExpr, _)),
          (barLoc, et.Lambda(List(aLoc, bLoc), None, barExpr, _))
        ), bodyExpr), _) =>
          assert(fooExpr === et.Apply(et.VarRef(barLoc), et.VarRef(xLoc) :: et.VarRef(yLoc) :: Nil))
          assert(barExpr === et.Cond(et.VarRef(aLoc), et.VarRef(bLoc), et.Literal(ast.UnitValue())))

          assert(bodyExpr === et.Apply(et.VarRef(fooLoc), et.Literal(ast.BooleanLiteral(true)) :: Nil))
    }
  }

  test("argless lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-true) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(Nil, None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }
  
  test("untyped one arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-true unused-param) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(List(fixedArg), None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(fixedArg.hasTypeConstraints === false)
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }
  
  test("typed one arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define: (return-true (unused-param : <symbol-cell>)) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(List(fixedArg), None, _, _)))) if procLoc == storageLoc =>
        assert(fixedArg.schemeType === vt.IntrinsicCellType(ct.SymbolCell))
    }
  }

  test("untyped fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val expr = exprFor("(define (return-false some . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(List(fixedArg), Some(restArg), bodyExpr, _)))) if procLoc == storageLoc =>
        assert(fixedArg.hasTypeConstraints === false)
        assert(restArg.hasTypeConstraints === false)

        assert(bodyExpr === et.Literal(ast.BooleanLiteral(false)))
    }

    inside(scope.get("return-false").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-false")
    }
  }
  
  test("typed fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    val expr = exprFor("(define: (return-false (some : <boolean-cell>) . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(List(fixedArg), Some(restArg), _, _)))) if procLoc == storageLoc =>
        assert(fixedArg.schemeType === vt.IntrinsicCellType(ct.BooleanCell))
        assert(restArg.hasTypeConstraints === false)
    }
  }
    
  test("untyped rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    val expr = exprFor("(define (return-six . rest) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(Nil, Some(restArg), bodyExpr, _)))) if procLoc == storageLoc =>
        assert(restArg.hasTypeConstraints === false)
        assert(bodyExpr === et.Literal(ast.IntegerLiteral(6)))
    }
    
    inside(scope.get("return-six").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-six")
    }
  }
    
  test("typed rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    val expr = exprFor("(define: (return-six . rest) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(Nil, Some(restArg), _, _)))) if procLoc == storageLoc =>
        assert(restArg.hasTypeConstraints === false)
    }
  }

  test("recursive lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-self) return-self)")(scope)
    val procLoc = scope.get("return-self").value
    inside(expr) {
      case et.TopLevelDefine(List((storageLoc, et.Lambda(Nil, None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(bodyExpr === et.VarRef(storageLoc))
    }
  }
  
  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      exprFor("(lambda (x x) x)")
    }
  }

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor("(define x 1)(lambda (x) x)")(scope)
    inside(expressions) {
      case List(et.TopLevelDefine(List((shadowed, _))), et.Lambda(List(argX), None, et.VarRef(inner), _)) =>
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
      case List(et.TopLevelDefine(List((shadowed, _))), et.Lambda(Nil, None, et.InternalDefine(List((inner, _)), _), _)) =>
        assert(inner != shadowed)
    }
  }
  
  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define y 1)(lambda (x) y)")(scope)

    inside(expressions) {
      case List(et.TopLevelDefine(List((outer, _))), et.Lambda(List(argX), None, et.VarRef(inner), _)) =>
        assert(outer === inner)
    }
  }

  test("define report procedure") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define-report-procedure list (lambda () #f))")(scope)
    val listBinding = scope.get("list").value

    inside(listBinding) {
      case rp : ReportProcedure =>
        assert(rp.reportName === "list")
    }

    inside(expr) {
      case et.TopLevelDefine(List((loc, et.Lambda(Nil, None, et.Literal(ast.BooleanLiteral(false)), _)))) =>
        assert(loc === listBinding)
    }
  }

  test("trivial include") {
    // Simple include should return an et.Begin with the contents of the ifle
    assert(exprFor("""(include "includes/include1.scm")""") ===
      et.Begin(List(
        et.Literal(ast.StringLiteral("include1-line1")), 
        et.Literal(ast.StringLiteral("include1-line2"))
      ))
    )
  }

  test("include multiple files in order") {
    // (include) with multiple files should read them in order and wrap them
    // in a single et.Begin
    assert(exprFor("""(include "includes/include1.scm" "includes/include2.scm")""") ===
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
    val expression = exprFor("""(include "includes/definea.scm")""")(scope)

    inside(scope("a")) {
      case storageLocA : StorageLocation =>
        inside(scope("b")) {
          case storageLocB : StorageLocation =>
            assert(expression === 
              et.Begin(List(
                et.TopLevelDefine(List(storageLocA -> et.Literal(ast.IntegerLiteral(1)))),
                et.Begin(List(
                  et.TopLevelDefine(List(storageLocB -> et.Literal(ast.IntegerLiteral(2)))),
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
  
  test("annotate expression types") {
    assert(exprFor("(ann #t <boolean-cell>)")(nfiScope) === 
      et.Cast(et.Literal(ast.BooleanLiteral(true)), vt.IntrinsicCellType(ct.BooleanCell))
    )
  
    intercept[BadSpecialFormException] {
      // No args
      exprFor("(ann #t)")(nfiScope)
    }
    
    intercept[BadSpecialFormException] {
      // Too many args
      exprFor("(ann #t <datum-cell> <string-cell>)")(nfiScope)
    }
    
    intercept[UnboundVariableException] {
      // Not a type
      exprFor("(ann #t <not-a-type>)")(nfiScope)
    }
    
    intercept[BadSpecialFormException] {
      // Native type
      exprFor("(ann #t <int64>)")(nfiScope)
    }
  }
  
  test("cond-expand with no clauses fails") {
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    intercept[BadSpecialFormException] {
      exprFor("""(cond-expand)""") 
    }
  }
  
  test("cond-expand with one true clause") {
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    assert(exprFor("""(cond-expand ((library (scheme base)) 1 2 3))""") ===
      et.Begin(List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Literal(ast.IntegerLiteral(2)),
        et.Literal(ast.IntegerLiteral(3))
      ))
    )
  }
  
  test("cond-expand with one false clause") {
    // ModuleBodyExtractor removes the empty et.Begin
    assert(bodyFor("""(cond-expand (not-a-feature 1 2 3))""")(primitiveScope) === Nil)
  }
  
  test("cond-expand with one false with else") {
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    assert(exprFor("""(cond-expand ((not r7rs) 1 2 3) (else 4 5 6))""") ===
      et.Begin(List(
        et.Literal(ast.IntegerLiteral(4)),
        et.Literal(ast.IntegerLiteral(5)),
        et.Literal(ast.IntegerLiteral(6))
      ))
    )
  }
  
  test("parameterize with no arguments fails") {
    intercept[BadSpecialFormException] {
      exprFor("""(parameterize)""")
    }
  }
  
  test("parameterize with three valued parameter fails") {
    intercept[BadSpecialFormException] {
      exprFor("""(parameterize (('param 'value 'extra)) #t)""")
    }
  }

  test("parameterize with no parameters") {
    assert(exprFor("""(parameterize () #t)""") ===
      et.Parameterize(
        Nil,
        et.Literal(ast.BooleanLiteral(true))
      )
    )
  }
  
  test("parameterize with two parameters") {
    assert(exprFor("""(parameterize (('param1 'value1) ('param2 'value2)) #t)""") ===
      et.Parameterize(
        List(
          (et.Literal(ast.Symbol("param1")), et.Literal(ast.Symbol("value1"))),
          (et.Literal(ast.Symbol("param2")), et.Literal(ast.Symbol("value2")))
        ),
        et.Literal(ast.BooleanLiteral(true))
      )
    )
  }
  
  test("parameterize with multiple body expressions") {
    assert(exprFor("""(parameterize (('param1 'value1)) 1 2 3)""") ===
      et.Parameterize(
        List(
          (et.Literal(ast.Symbol("param1")), et.Literal(ast.Symbol("value1")))
        ),
        et.Begin(List(
          et.Literal(ast.IntegerLiteral(1)),
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        ))
      )
    )
  }
  
  test("parameterize introduces a body context") {
    inside(exprFor("""(parameterize () (define a 1))""")) { 
      case et.Parameterize(Nil, et.InternalDefine(_, _)) =>
        // Checks out
    }
  }
}
