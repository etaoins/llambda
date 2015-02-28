package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._

class ExtractModuleBodySuite extends FunSuite with Inside with OptionValues with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  
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

  test("(begin) in top-level context") {
    assert(bodyFor("(begin 1 2)")(primitiveScope) === List(
      et.Literal(ast.IntegerLiteral(1)),
      et.Literal(ast.IntegerLiteral(2))
    ))
  }

  test("(begin) in expression context") {
    inside(exprFor("(+ (begin 1 2))")(plusScope)) {
      case et.Apply(
        et.VarRef(plusLoc),
        List(et.Begin(List(
          et.Literal(ast.IntegerLiteral(1)),
          et.Literal(ast.IntegerLiteral(2))
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
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2)))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(storageLoc.schemeType == vt.AnySchemeType)
    }
  }

  test("define untyped fixed-only multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define-values (x y) 2)")(scope)

    val storageLocX = scope.get("x").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocY = scope.get("y").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocX.sourceName === "x")
    assert(storageLocX.schemeType === vt.AnySchemeType)

    assert(storageLocY.sourceName === "y")
    assert(storageLocY.schemeType === vt.AnySchemeType)

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(List(storageLocX, storageLocY), None, et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }

  test("define untyped rest-only multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define-values r 2)")(scope)

    val storageLocR = scope.get("r").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocR.sourceName === "r")
    assert(storageLocR.schemeType === vt.UniformProperListType(vt.AnySchemeType))

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(Nil, Some(storageLocR), et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }


  test("define annotated fixed and rest multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("""
      (: x <string>)
      (define-values (x y . r) 2)
    """)(scope)

    val storageLocX = scope.get("x").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocY = scope.get("y").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocR = scope.get("r").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocX.sourceName === "x")
    assert(storageLocX.schemeType === vt.StringType)

    assert(storageLocY.sourceName === "y")
    assert(storageLocY.schemeType === vt.AnySchemeType)

    assert(storageLocR.sourceName === "r")
    assert(storageLocR.schemeType === vt.UniformProperListType(vt.AnySchemeType))

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(List(storageLocX, storageLocY), Some(storageLocR), et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }

  test("define typed fixed-only multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("(define-values ([x : <string>] [y : <symbol>]) 2)")(scope)

    val storageLocX = scope.get("x").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocY = scope.get("y").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocX.sourceName === "x")
    assert(storageLocX.schemeType === vt.StringType)

    assert(storageLocY.sourceName === "y")
    assert(storageLocY.schemeType === vt.SymbolType)

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(List(storageLocX, storageLocY), None, et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }

  test("define typed rest-only multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("(define-values (r : <port> *) 2)")(scope)

    val storageLocR = scope.get("r").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocR.sourceName === "r")
    assert(storageLocR.schemeType === vt.UniformProperListType(vt.PortType))

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(Nil, Some(storageLocR), et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }

  test("define typed fixed and rest multiple values") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("""
      (: r (Listof <exact-integer>))
      (define-values ([x : <string>] [y : <symbol>] r : <exact-integer> *) 2)
    """)(scope)

    val storageLocX = scope.get("x").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocY = scope.get("y").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    val storageLocR = scope.get("r").value match {
      case storageLoc : StorageLocation => storageLoc
      case _ => fail("Unexpected bound value")
    }

    assert(storageLocX.sourceName === "x")
    assert(storageLocX.schemeType === vt.StringType)

    assert(storageLocY.sourceName === "y")
    assert(storageLocY.schemeType === vt.SymbolType)

    assert(storageLocR.sourceName === "r")
    assert(storageLocR.schemeType === vt.UniformProperListType(vt.ExactIntegerType))

    assert(expressions === List(
      et.TopLevelDefine(List(
        et.Binding(List(storageLocX, storageLocY), Some(storageLocR), et.Literal(ast.IntegerLiteral(2)))
      ))
    ))
  }

  test("define typed variable") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor("(define a : <exact-integer> 2)")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2)))))
        ))

        // Make sure we preserved our source name for debugging purposes
        assert(storageLoc.sourceName === "a")
        assert(storageLoc.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("define unstable typed variable fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    // Because R7RS uses mutable pairs the (Listof) type isn't stable
    intercept[BadSpecialFormException] {
      bodyFor("(define a : (Listof <exact-integer>) '(1 2 3 4))")(scope, dialect.R7RS)
    }
  }
  
  test("define type declared variable") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor(
      """(: a <exact-integer>)
         (define a 2)""")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2)))))
        ))

        assert(storageLoc.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("multiple compatible type declarations are allowed") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val expressions = bodyFor(
      """(: a <exact-integer>)
         (: a <exact-integer>)
         (define a 2)""")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2)))))
        ))

        assert(storageLoc.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("type declaration without define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor("""(: a <exact-integer>)""")(scope)
    }
  }

  test("type declaration for already bound value fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define a 1)
           (: a <exact-integer>)""")(scope)
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
  
  test("redefine variable succeeds in R7RS") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define a 3)")(scope, dialect.R7RS)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2))))),
          et.MutateVar(storageLoc, et.Literal(ast.IntegerLiteral(3)))
        ))
    }
  }
  
  test("redefine variable fails in llambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[DuplicateDefinitionException] {
      bodyFor("(define a 2)(define a 3)")(scope, dialect.Llambda)
    }
  }

  test("redefine variable using (define-values) fails") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[DuplicateDefinitionException] {
      bodyFor("(define a 2)(define-values (a c) (values 1 3))")(scope)
    }
  }
  
  test("dependent define") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2)(define b a)")(scope)

    inside((scope.get("a").value, scope.get("b").value)) {
      case (storageLocA : StorageLocation, storageLocB : StorageLocation) =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLocA, et.Literal(ast.IntegerLiteral(2))))),
          et.TopLevelDefine(List(et.SingleBinding(storageLocB, et.VarRef(storageLocA))))
        ))
    }
  }
  
  test("reference variable") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define a 2) a")(scope)

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(expressions === List(
          et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(ast.IntegerLiteral(2))))),
          et.VarRef(storageLoc)
        ))
    }
  } 

  test("parameters shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor("(define x 1)(lambda (x) x)")(scope)
    inside(expressions) {
      case List(et.TopLevelDefine(List(et.SingleBinding(shadowed, _))), et.Lambda(_, List(argX), None, et.VarRef(inner), _)) =>
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
      case List(et.TopLevelDefine(List(et.SingleBinding(shadowed, _))), et.Lambda(_, Nil, None, et.InternalDefine(List(et.SingleBinding(inner, _)), _), _)) =>
        assert(inner != shadowed)
    }
  }

  test("splicing (begin) in lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expressions = bodyFor(
      """(lambda () (begin (define x 2)))"""
    )(scope)

    inside(expressions) {
      case List(et.Lambda(_, Nil, None, et.InternalDefine(List(et.SingleBinding(inner, _)), _), _)) =>
    }
  }

  test("type declaration can shadow") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(define x 'foo)
         (lambda ()
           (: x <exact-integer>)
           (define x 2))"""
    )(scope) 

    inside(expressions) {
      case List(et.TopLevelDefine(List(et.SingleBinding(shadowed, _))), et.Lambda(_, Nil, None, et.InternalDefine(List(et.SingleBinding(inner, _)), _), _)) =>
        assert(inner != shadowed)

        assert(shadowed.schemeType === vt.AnySchemeType)
        assert(inner.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("type declaration of internal definition") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(lambda ()
         (: x <exact-integer>)
         (define x 2))"""
    )(scope) 

    inside(expressions) {
      case List( et.Lambda(_, Nil, None, et.InternalDefine(List(et.SingleBinding(inner, _)), _), _)) =>
        assert(inner.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("type declaration of internal definition with incompatible type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(lambda ()
           (define x : <flonum> 4.0)
           (: x <exact-integer>))"""
      )(scope) 
    }
  }
  
  test("type declaring compatible typed define") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expressions = bodyFor(
      """(lambda ()
         (: x <exact-integer>)
         (define x : <exact-integer> 2))"""
    )(scope)

    inside(expressions) {
      case List( et.Lambda(_, Nil, None, et.InternalDefine(List(et.SingleBinding(inner, _)), _), _)) =>
        assert(inner.schemeType === vt.ExactIntegerType)
    }
  }
  
  test("type declaring incompatible typed define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(lambda ()
           (: x <exact-integer>)
           (define x : <flonum> 2))"""
      )(scope) 
    }
  }
  
  test("type declaration wihout internal define fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor(
        """(lambda ()
           (: x <exact-integer>))"""
      )(scope) 
    }
  }
  
  test("type declaration from outer scope fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[UnboundVariableException] {
      bodyFor(
        """(: x <exact-integer>)
           (lambda ()
           (define x 2))"""
      )(scope) 
    }
  }
  
  
  test("capturing") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("(define y 1)(lambda (x) y)")(scope)

    inside(expressions) {
      case List(et.TopLevelDefine(List(et.SingleBinding(outer, _))), et.Lambda(_, List(argX), None, et.VarRef(inner), _)) =>
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
      case et.TopLevelDefine(List(et.SingleBinding(loc, et.Lambda(_, Nil, None, et.Literal(ast.BooleanLiteral(false)), _)))) =>
        assert(loc === listBinding)
    }
  }

  test("trivial include") {
    // Simple include should return an et.Begin with the contents of the ifle
    assert(bodyFor("""(include "includes/include1.scm")""")(primitiveScope) ===
      List(
        et.Literal(ast.StringLiteral("include1-line1")),
        et.Literal(ast.StringLiteral("include1-line2"))
      )
    )
  }

  test("include multiple files in order") {
    // (include) with multiple files should read them in order and wrap them
    // in a single et.Begin
    assert(bodyFor("""(include "includes/include1.scm" "includes/include2.scm")""")(primitiveScope) ===
      List(
        et.Literal(ast.StringLiteral("include1-line1")),
        et.Literal(ast.StringLiteral("include1-line2")),
        et.Literal(ast.StringLiteral("include2-line1")),
        et.Literal(ast.StringLiteral("include2-line2"))
      )
    )
  }

  test("include handles relative includes and scope correctly") {
    // This tests both relative (include)s and that scoping works correctly
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    val expressions = bodyFor("""(include "includes/definea.scm")""")(scope)

    inside(scope("a")) {
      case storageLocA : StorageLocation =>
        inside(scope("b")) {
          case storageLocB : StorageLocation =>
            assert(expressions ===
              List(
                et.TopLevelDefine(List(et.SingleBinding(storageLocA, et.Literal(ast.IntegerLiteral(1))))),
                et.TopLevelDefine(List(et.SingleBinding(storageLocB, et.Literal(ast.IntegerLiteral(2))))),
                et.VarRef(storageLocA),
                et.VarRef(storageLocB)
              )
            )
        }
    }
  }

  test("include-ci") {
    // Simple include should return an et.Begin with the contents of the ifle
    assert(exprFor("""(include-ci "includes/vector-include.scm")""") ===
      et.Literal(ast.VectorLiteral(Vector(
        ast.Symbol("upper"),
        ast.Symbol("mixed"),
        ast.Symbol("lower")
      )))
    )
  }

  test("cast expression types") {
    assert(exprFor("(cast #t <boolean>)")(nfiScope) === 
      et.Cast(et.Literal(ast.BooleanLiteral(true)), vt.BooleanType, false)
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
      et.Cast(et.Literal(ast.BooleanLiteral(true)), vt.BooleanType, true)
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
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    assert(bodyFor("""(cond-expand ((library (scheme base)) 1 2 3))""")(primitiveScope) ===
      List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Literal(ast.IntegerLiteral(2)),
        et.Literal(ast.IntegerLiteral(3))
      )
    )
  }

  test("cond-expand with one false clause") {
    // ExtractModuleBody removes the empty et.Begin
    assert(bodyFor("""(cond-expand (not-a-feature 1 2 3))""")(primitiveScope) === Nil)
  }

  test("cond-expand with one false with else") {
    // We would normally expand this to an empty et.Begin but it's disallowed by R7RS
    assert(bodyFor("""(cond-expand ((not llambda) 1 2 3) (else 4 5 6))""")(primitiveScope) ===
      List(
        et.Literal(ast.IntegerLiteral(4)),
        et.Literal(ast.IntegerLiteral(5)),
        et.Literal(ast.IntegerLiteral(6))
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
  
  test("creating type predicate from a procedure type fails") {
    intercept[BadSpecialFormException] {
      exprFor("(make-predicate (-> <symbol> <symbol>))")(nfiScope)
    }
  }

  test("creating type predicate from a Scheme type") {
    assert(exprFor("(make-predicate <symbol>)")(nfiScope) === 
      et.TypePredicate(vt.SymbolType)
    )
  }
}
