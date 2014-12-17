package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}
import llambda.compiler.valuetype.Implicits._

class ExtractLambdaSuite extends FunSuite with Inside with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))

  test("type declaration for untyped lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (-> <string> <symbol>))
         (define (string-to-symbol x) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }

    inside(exprFor(
      """(: strings-to-symbol (-> <string> <string> * <symbol>))
         (define (strings-to-symbol x . rest) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("polymorphic type declaration for untyped lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (All ([A : <number>]) A <symbol>))
         (define (string-to-symbol x) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _)))) =>
        inside(polyType) {
          case pm.PolymorphicProcedureType(typeVars,
            vt.ProcedureType(List(polyVarA : pm.TypeVar), None, vt.ReturnType.SingleValue(vt.SymbolType))
          ) =>
            assert(polyVarA.upperBound === vt.NumberType)
            assert(typeVars === Set(polyVarA))
        }

        assert(storageLoc.schemeType ===
          vt.ProcedureType(
            fixedArgTypes=List(vt.NumberType),
            restArgMemberTypeOpt=None,
            returnType=vt.ReturnType.SingleValue(vt.SymbolType)
          )
        )
    }
  }

  test("type declaration for untyped lambda with incompatible fixed arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <symbol>))
           (define (string-to-symbol x y) x)"""
      )(scope)
    }
  }

  test("type declaration for untyped lambda with incompatible rest arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <string> * <symbol>))
           (define (string-to-symbol x) x)"""
      )(scope)
    }

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <string> <symbol>))
           (define (string-to-symbol x . rest) x)"""
      )(scope)
    }
  }

  test("polymorphic type declaration mixed with argument types fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (All (A) A <symbol>))
           (define (string-to-symbol [x : <exact-integer>]) x)"""
      )(scope)
    }
  }

  test("type declaration for typed lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (-> <string> <symbol>))
         (define (string-to-symbol [x : <string>]) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }

    inside(exprFor(
      """(: strings-to-symbol (-> <string> <string> * <symbol>))
         (define (strings-to-symbol [x : <string>] rest : <string> *) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("type declaration for typed lambda uses most specific fixed arg type") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: le-to-symbol (-> <list-element> <symbol>))
         (define (le-to-symbol [x : <pair>]) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedStorageLocType = vt.ProcedureType(
          fixedArgTypes=List(vt.ListElementType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        val expectedLambdaType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnyPairType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedStorageLocType)
        assert(polyType === expectedLambdaType.toPolymorphic)
    }

    inside(exprFor(
      """(: pair-to-symbol (-> <pair> <symbol>))
         (define (pair-to-symbol [x : <list-element>]) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnyPairType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("type declaration for typed lambda uses most specific rest arg type") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: le-to-symbol (-> <list-element> * <symbol>))
         (define (le-to-symbol x : <pair> *) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedStorageLocType = vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.ListElementType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        val expectedLambdaType = vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnyPairType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedStorageLocType)
        assert(polyType === expectedLambdaType.toPolymorphic)
    }

    inside(exprFor(
      """(: pair-to-symbol (-> <pair> * <symbol>))
         (define (pair-to-symbol x : <list-element> *) x)"""
    )(scope)) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _))))  =>
        val expectedType = vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnyPairType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("type declaration for typed lambda with incompatible fixed arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <symbol>))
           (define (string-to-symbol [x : <string>] [y : <string>]) x)"""
      )(scope)
    }
  }

  test("type declaration for typed lambda with incompatible fixed arg type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <symbol>))
           (define (string-to-symbol [x : <symbol>]) x)"""
      )(scope)
    }
  }

  test("type declaration for typed lambda with incompatible rest arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <string> * <symbol>))
           (define (string-to-symbol [x : <string>]) x)"""
      )(scope)
    }

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <string> <symbol>))
           (define (string-to-symbol [x : <string>] rest : <string> *) x)"""
      )(scope)
    }
  }

  test("type declaration for typed lambda with incompatible rest arg type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> * <symbol>))
           (define (string-to-symbol x : <symbol> *) x)"""
      )(scope)
    }
  }

  test("untyped lambdas") {
    inside(exprFor("(lambda () #t)")) {
      case et.Lambda(polyType, Nil, None, body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(exprFor("(lambda (x) x)")) {
      case et.Lambda(polyType, List(argX), None, body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(argX.schemeType === vt.AnySchemeType)
        assert(body === et.VarRef(argX))
    }

    inside(exprFor("(lambda x x)")) {
      case et.Lambda(polyType, Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda (x y . z) x y z)")) {
      case et.Lambda(polyType, List(argX, argY), Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType, vt.AnySchemeType),
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(argX.schemeType === vt.AnySchemeType)
        assert(argY.schemeType === vt.AnySchemeType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(body === et.Begin(List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        )))
    }
  }

  test("typed lambdas") {
    inside(exprFor("(lambda ([x : <number>]) x)")(nfiScope)) {
      case et.Lambda(polyType, List(argX), None, body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.NumberType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(argX.schemeType === vt.NumberType)
        assert(body === et.VarRef(argX))
    }

    inside(exprFor("(lambda (x : <any> *) x)")(nfiScope)) {
      case et.Lambda(polyType, Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda ([x : <exact-integer>] y z : <symbol> *) x y z)")(nfiScope)) {
      case et.Lambda(polyType, List(argX, argY), Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.ExactIntegerType, vt.AnySchemeType),
          restArgMemberTypeOpt=Some(vt.SymbolType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(argX.schemeType === vt.ExactIntegerType)
        assert(argY.schemeType === vt.AnySchemeType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.SymbolType))

        assert(body === et.Begin(List(
          et.VarRef(argX),
          et.VarRef(argY),
          et.VarRef(restArg)
        )))
    }
  }

  test("self-executing lambdas") {
    inside(exprFor("((lambda (x) x) 1)")) {
      case et.Apply(et.Lambda(polyType, List(argX), None, body, _), List(value)) =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

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
      case et.Lambda(_, List(xLoc), None, et.InternalDefine(
        List(
          et.SingleBinding(fooLoc, et.Lambda(_, List(yLoc), None, fooExpr, _)),
          et.SingleBinding(barLoc, et.Lambda(_, List(aLoc, bLoc), None, barExpr, _))
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
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

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
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.AnySchemeType)
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }

  test("typed one arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define (return-true [unused-param : <symbol>]) #t)")(scope)
    val procLoc = scope.get("return-true").value

    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), None, _, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.SymbolType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.SymbolType)
    }
  }

  test("untyped fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-false some . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Some(restArg), bodyExpr, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.AnySchemeType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(bodyExpr === et.Literal(ast.BooleanLiteral(false)))
    }

    inside(scope.get("return-false").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-false")
    }
  }

  test("typed fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define (return-false [some : <boolean>] rest : <string> *) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Some(restArg), _, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.BooleanType),
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.BooleanType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.StringType))
    }
  }

  test("(define-report-procedure) lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define-report-procedure (return-false [some : <boolean>] rest : <string> *) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(procLoc) {
      case rp : ReportProcedure =>
        assert(rp.reportName === "return-false")
    }

    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Some(restArg), _, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=List(vt.BooleanType),
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.BooleanType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.StringType))
    }
  }

  test("untyped rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-six . rest) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Some(restArg), bodyExpr, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))
        assert(bodyExpr === et.Literal(ast.IntegerLiteral(6)))
    }

    inside(scope.get("return-six").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-six")
    }
  }

  test("typed rest only arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define (return-six rest : <port> *) 6)")(scope)
    val procLoc = scope.get("return-six").value
    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Some(restArg), _, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.PortType),
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.PortType))
    }
  }

  test("recursive lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-self) return-self)")(scope)
    val procLoc = scope.get("return-self").value
    inside(expr) {
      case et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, None, bodyExpr, _)))) if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        ).toPolymorphic)

        assert(bodyExpr === et.VarRef(storageLoc))
    }
  }

  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      exprFor("(lambda (x x) x)")
    }
  }
}
