package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}


class ExtractModuleBodySuite extends FunSuite with Inside with OptionValues with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq: _*))

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
      ast.Vector(Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c")))
    ))

    assert(exprFor("'()") === et.Literal(ast.EmptyList()))

    assert(exprFor("'(+ 1 2)") === et.Literal(
      ast.ProperList(List(ast.Symbol("+"), ast.Integer(1), ast.Integer(2)))
    ))

    assert(exprFor("'(quote a)") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))

    assert(exprFor("''a") === et.Literal(
      ast.ProperList(List(ast.Symbol("quote"), ast.Symbol("a")))
    ))

    assert(exprFor("'145932") === et.Literal(
      ast.Integer(145932)
    ))

    assert(exprFor("145932") === et.Literal(
      ast.Integer(145932)
    ))

    assert(exprFor("'\"" + "abc" + "\"") === et.Literal(
      ast.String("abc")
    ))

    assert(exprFor("\"" + "abc" + "\"") === et.Literal(
      ast.String("abc")
    ))

    assert(exprFor("'#(a 10)") === et.Literal(
      ast.Vector(Vector(ast.Symbol("a"), ast.Integer(10)))
    ))

    assert(exprFor("#(a 10)") === et.Literal(
      ast.Vector(Vector(ast.Symbol("a"), ast.Integer(10)))
    ))

    assert(exprFor("'#u8(64 65)") === et.Literal(
      ast.Bytevector(Vector(64, 65))
    ))

    assert(exprFor("#u8(64 65)") === et.Literal(
      ast.Bytevector(Vector(64, 65))
    ))

    assert(exprFor("'#t") === et.Literal(
      ast.Boolean(true)
    ))

    assert(exprFor("#t") === et.Literal(
      ast.Boolean(true)
    ))

    assert(exprFor("#!unit") === et.Literal(
      ast.Unit()
    ))
  }

  test("application") {
    assert(exprFor("(+ 3 4)")(plusScope) === et.Apply(
      et.VarRef(plusLoc),
      List(
        et.Literal(ast.Integer(3)),
        et.Literal(ast.Integer(4))
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

  test("(begin) in top-level context") {
    assert(bodyFor("(begin 1 2)")(primitiveScope) === List(
      et.Literal(ast.Integer(1)),
      et.Literal(ast.Integer(2))
    ))
  }

  test("(begin) in expression context") {
    inside(exprFor("(+ (begin 1 2))")(plusScope)) {
      case et.Apply(
        et.VarRef(plusLoc),
        List(et.Begin(List(
          et.Literal(ast.Integer(1)),
          et.Literal(ast.Integer(2))
        )))
      ) =>
    }
  }

  test("set!") {
    // Make a storage location for a
    val storageLoc = new StorageLocation("a")
    val varScope = new Scope(collection.mutable.Map("a" -> storageLoc), Some(primitiveScope))

    assert(exprFor("(set! a 1)")(varScope) === et.MutateVar(
      storageLoc,
      et.Literal(ast.Integer(1))
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
      et.Literal(ast.Boolean(true)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.Symbol("no"))
    ))

    assert(exprFor("(if #f 'yes)") === et.Cond(
      et.Literal(ast.Boolean(false)),
      et.Literal(ast.Symbol("yes")),
      et.Literal(ast.Unit())
    ))
  }

  test("define untyped variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)")(scope)

    inside(scope.get("a").value) {
      case storageLoc: StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLoc, et.Literal(ast.Integer(2))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(storageLoc.schemeType == vt.AnySchemeType)
    }
  }

  test("define typed variable") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("(define a : <integer> 2)")(scope)

    inside(scope.get("a").value) {
      case storageLoc: StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLoc, et.Literal(ast.Integer(2))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(storageLoc.schemeType === vt.IntegerType)
    }
  }

  test("define type declared variable") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor(
      """(: a <integer>)
         (define a 2)""")(scope)

    inside(scope.get("a").value) {
      case storageLoc: StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLoc, et.Literal(ast.Integer(2))))
        ))

        assert(storageLoc.schemeType === vt.IntegerType)
    }
  }

  test("multiple compatible type declarations are allowed") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor(
      """(: a <integer>)
         (: a <integer>)
         (define a 2)""")(scope)

    inside(scope.get("a").value) {
      case storageLoc: StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLoc, et.Literal(ast.Integer(2))))
        ))

        assert(storageLoc.schemeType === vt.IntegerType)
    }
  }

  test("type declaration without define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor("""(: a <integer>)""")(scope)
    }
  }

  test("type declaration for already bound value fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define a 1)
           (: a <integer>)""")(scope)
    }
  }

  test("type declaration with unbound type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor(
        """(: a <not-a-type>)
           (define a 1)""")(scope)
    }
  }

  test("redefine variable fails") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[DuplicateDefinitionException] {
      bodyFor("(define a 2)(define a 3)")(scope)
    }
  }

  test("dependent define") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define b a)")(scope)

    inside((scope.get("a").value, scope.get("b").value)) {
      case (storageLocA: StorageLocation, storageLocB: StorageLocation) =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLocA, et.Literal(ast.Integer(2)))),
          et.TopLevelDefine(et.Binding(storageLocB, et.VarRef(storageLocA)))
        ))
    }
  }

  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2) a")(scope)

    inside(scope.get("a").value) {
      case storageLoc: StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(et.Binding(storageLoc, et.Literal(ast.Integer(2)))),
          et.VarRef(storageLoc)
        ))
    }
  }

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor("(define x 1)(lambda (x) x)")(scope)
    inside(expressions) {
      case List(et.TopLevelDefine(et.Binding(shadowed, _)), et.Lambda(_, List(argX), Nil, None, et.VarRef(inner), _)) =>
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
      case List(et.TopLevelDefine(et.Binding(shadowed, _)), et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(et.Binding(inner, _)), _), _)) =>
        assert(inner != shadowed)
    }
  }

  test("splicing (begin) in lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor(
      """(lambda () (begin (define x 2)))"""
    )(scope)

    inside(expressions) {
      case List(et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(et.Binding(inner, _)), _), _)) =>
    }
  }

  test("type declaration can shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(define x 'foo)
         (lambda ()
           (: x <integer>)
           (define x 2))"""
    )(scope)

    inside(expressions) {
      case List(et.TopLevelDefine(et.Binding(shadowed, _)), et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(et.Binding(inner, _)), _), _)) =>
        assert(inner != shadowed)

        assert(shadowed.schemeType === vt.AnySchemeType)
        assert(inner.schemeType === vt.IntegerType)
    }
  }

  test("type declaration of internal definition") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(lambda ()
         (: x <integer>)
         (define x 2))"""
    )(scope)

    inside(expressions) {
      case List( et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(et.Binding(inner, _)), _), _)) =>
        assert(inner.schemeType === vt.IntegerType)
    }
  }

  test("type declaration of internal definition with incompatible type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(lambda ()
           (define x : <flonum> 4.0)
           (: x <integer>))"""
      )(scope)
    }
  }

  test("type declaring compatible typed define") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(lambda ()
         (: x <integer>)
         (define x : <integer> 2))"""
    )(scope)

    inside(expressions) {
      case List( et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(et.Binding(inner, _)), _), _)) =>
        assert(inner.schemeType === vt.IntegerType)
    }
  }

  test("type declaring incompatible typed define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(lambda ()
           (: x <integer>)
           (define x : <flonum> 2))"""
      )(scope)
    }
  }

  test("type declaration wihout internal define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor(
        """(lambda ()
           (: x <integer>))"""
      )(scope)
    }
  }

  test("type declaration from outer scope fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor(
        """(: x <integer>)
           (lambda ()
           (define x 2))"""
      )(scope)
    }
  }


  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define y 1)(lambda (x) y)")(scope)

    inside(expressions) {
      case List(et.TopLevelDefine(et.Binding(outer, _)), et.Lambda(_, List(argX), Nil, None, et.VarRef(inner), _)) =>
        assert(outer === inner)
    }
  }

  test("define stdlib procedure") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define-stdlib-procedure list (lambda () #f))")(scope)
    val listBinding = scope.get("list").value

    inside(listBinding) {
      case sp: StdlibProcedure =>
        assert(sp.stdlibName === "list")
    }

    inside(expr) {
      case et.TopLevelDefine(et.Binding(loc, et.Lambda(_, Nil, Nil, None, et.Literal(ast.Boolean(false)), _))) =>
        assert(loc === listBinding)
    }

    // (define-stdlib-procedure) bindings should be immutable
    intercept[BadSpecialFormException] {
      bodyFor("(set! list 4)")(scope)
    }
  }

  test("trivial include") {
    // Simple include should return an et.Begin with the contents of the file
    assert(bodyFor("""(include "includes/include1.scm")""")(primitiveScope) ===
      List(
        et.Literal(ast.String("include1-line1")),
        et.Literal(ast.String("include1-line2"))
      )
    )
  }

  test("include multiple files in order") {
    // (include) with multiple files should read them in order and wrap them
    // in a single et.Begin
    assert(bodyFor("""(include "includes/include1.scm" "includes/include2.scm")""")(primitiveScope) ===
      List(
        et.Literal(ast.String("include1-line1")),
        et.Literal(ast.String("include1-line2")),
        et.Literal(ast.String("include2-line1")),
        et.Literal(ast.String("include2-line2"))
      )
    )
  }

  test("include handles relative includes and scope correctly") {
    // This tests both relative (include)s and that scoping works correctly
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("""(include "includes/definea.scm")""")(scope)

    inside(scope("a")) {
      case storageLocA: StorageLocation =>
        inside(scope("b")) {
          case storageLocB: StorageLocation =>
            assert(expressions ===
              List(
                et.TopLevelDefine(et.Binding(storageLocA, et.Literal(ast.Integer(1)))),
                et.TopLevelDefine(et.Binding(storageLocB, et.Literal(ast.Integer(2)))),
                et.VarRef(storageLocA),
                et.VarRef(storageLocB)
              )
            )
        }
    }
  }

  test("include in lambda body context") {
    val expr = exprFor("""(lambda () (define c 3) (include "includes/definea.scm") c)""")

    inside(expr) {
      case et.Lambda(_, Nil, Nil, None, et.InternalDefine(List(
          et.Binding(bindLocC, et.Literal(ast.Integer(3))),
          et.Binding(bindLocA, et.Literal(ast.Integer(1))),
          et.Binding(bindLocB, et.Literal(ast.Integer(2)))
        ),
        et.Begin(List(
          et.VarRef(refLocA),
          et.VarRef(refLocB),
          et.VarRef(refLocC)
        ))
      ), _) =>
        assert(bindLocA === refLocA)
        assert(bindLocB === refLocB)
        assert(bindLocC === refLocC)
    }
  }

  test("include in expression context") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expr = exprFor("""(define d (include "includes/definea.scm"))""")(scope)

    inside(expr) {
      case et.TopLevelDefine(et.Binding(_, et.InternalDefine(List(
          et.Binding(bindLocA, et.Literal(ast.Integer(1))),
          et.Binding(bindLocB, et.Literal(ast.Integer(2)))
        ),
        et.Begin(List(
          et.VarRef(refLocA),
          et.VarRef(refLocB)
        ))
      ))) =>
        assert(bindLocA === refLocA)
        assert(bindLocB === refLocB)
    }
  }

  test("cast expression types") {
    assert(exprFor("(cast #t <boolean>)")(nfiScope) ===
      et.Cast(et.Literal(ast.Boolean(true)), vt.BooleanType, false)
    )

    intercept[BadSpecialFormException] {
      // No args
      exprFor("(cast #t)")(nfiScope)
    }

    intercept[BadSpecialFormException] {
      // Too many args
      exprFor("(cast #t <any> <string>)")(nfiScope)
    }

    intercept[UnboundVariableException] {
      // Not a type
      exprFor("(cast #t <not-a-type>)")(nfiScope)
    }

    intercept[BadSpecialFormException] {
      // Native type
      exprFor("(cast #t <native-int64>)")(nfiScope)
    }
  }

  test("annotation expression types") {
    assert(exprFor("(ann #t <boolean>)")(nfiScope) ===
      et.Cast(et.Literal(ast.Boolean(true)), vt.BooleanType, true)
    )

    intercept[BadSpecialFormException] {
      // No args
      exprFor("(ann #t)")(nfiScope)
    }

    intercept[BadSpecialFormException] {
      // Too many args
      exprFor("(ann #t <any> <string>)")(nfiScope)
    }

    intercept[UnboundVariableException] {
      // Not a type
      exprFor("(ann #t <not-a-type>)")(nfiScope)
    }

    intercept[BadSpecialFormException] {
      // Native type
      exprFor("(ann #t <native-int64>)")(nfiScope)
    }
  }

  test("cond-expand with no clauses fails") {
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    intercept[BadSpecialFormException] {
      exprFor("""(cond-expand)""")
    }
  }

  test("cond-expand with one true clause") {
    assert(bodyFor("""(cond-expand ((library (llambda base)) 1 2 3))""")(primitiveScope) ===
      List(
        et.Literal(ast.Integer(1)),
        et.Literal(ast.Integer(2)),
        et.Literal(ast.Integer(3))
      )
    )
  }

  test("cond-expand with one false clause") {
    // ExtractModuleBody removes the empty et.Begin
    assert(bodyFor("""(cond-expand (not-a-feature 1 2 3))""")(primitiveScope) === Nil)
  }

  test("cond-expand with one false with else") {
    assert(bodyFor("""(cond-expand ((not llambda) 1 2 3) (else 4 5 6))""")(primitiveScope) ===
      List(
        et.Literal(ast.Integer(4)),
        et.Literal(ast.Integer(5)),
        et.Literal(ast.Integer(6))
      )
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
        et.Literal(ast.Boolean(true))
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
        et.Literal(ast.Boolean(true))
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
          et.Literal(ast.Integer(1)),
          et.Literal(ast.Integer(2)),
          et.Literal(ast.Integer(3))
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

  test("creating type predicates without a type name fails") {
    intercept[BadSpecialFormException] {
      exprFor("(make-predicate)")(nfiScope)
    }
  }

  test("creating type predicates with non-type fails ") {
    intercept[BadSpecialFormException] {
      exprFor("(make-predicate 5)")(nfiScope)
    }
  }

  test("creating type predicate from a Scheme type") {
    assert(exprFor("(make-predicate <symbol>)")(nfiScope) ===
      et.TypePredicate(vt.SymbolType)
    )
  }
}
