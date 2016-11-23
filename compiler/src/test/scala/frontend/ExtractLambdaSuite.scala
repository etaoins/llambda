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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=Nil,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("type declaration for untyped optionals lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (->* (<string>) (<number>) <symbol>))
         (define (string-to-symbol x [y 1]) x)"""
    )(scope)) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=List(vt.NumberType),
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=Nil,
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
      """(: string-to-symbol (All ([A : <number>]) (-> A <symbol>)))
         (define (string-to-symbol x) x)"""
    )(scope)) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        inside(polyType) {
          case pm.PolymorphicProcedureType(typeVars,
            vt.ProcedureType(List(polyVarA : pm.TypeVar), Nil, None, vt.ReturnType.SingleValue(vt.SymbolType))
          ) =>
            assert(polyVarA.upperBound === vt.NumberType)
            assert(typeVars === Set(polyVarA))
        }

        assert(storageLoc.schemeType ===
          vt.ProcedureType(
            mandatoryArgTypes=List(vt.NumberType),
            optionalArgTypes=Nil,
            restArgMemberTypeOpt=None,
            returnType=vt.ReturnType.SingleValue(vt.SymbolType)
          )
        )
    }
  }

  test("polymorphic type declaration for untyped optionals lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (All ([A : <number>]) (->* () (A) <number>)))
         (define (string-to-symbol [x 5]) x)"""
    )(scope)) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        inside(polyType) {
          case pm.PolymorphicProcedureType(typeVars,
            vt.ProcedureType(Nil, List(polyVarA : pm.TypeVar), None, vt.ReturnType.SingleValue(vt.NumberType))
          ) =>
            assert(polyVarA.upperBound === vt.NumberType)
            assert(typeVars === Set(polyVarA))
        }

        assert(storageLoc.schemeType ===
          vt.ProcedureType(
            mandatoryArgTypes=Nil,
            optionalArgTypes=List(vt.NumberType),
            restArgMemberTypeOpt=None,
            returnType=vt.ReturnType.SingleValue(vt.NumberType)
          )
        )
    }
  }

  test("type declaration for untyped lambda with incompatible mandatory arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <symbol>))
           (define (string-to-symbol x y) x)"""
      )(scope)
    }
  }

  test("type declaration for untyped lambda with incompatible optional arg arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(: string-to-symbol (-> <string> <symbol>))
           (define (string-to-symbol x [y "Hello"]) x)"""
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=Nil,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.StringType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        assert(storageLoc.schemeType === expectedType)
        assert(polyType === expectedType.toPolymorphic)
    }
  }

  test("type declaration for typed optionals lambda with compatible arity") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(: string-to-symbol (->* () (<string>) <symbol>))
         (define (string-to-symbol [x : <string> "Hello"]) x)"""
    )(scope)) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=None,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedStorageLocType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.ListElementType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        val expectedLambdaType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnyPairType),
          optionalArgTypes=Nil,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnyPairType),
          optionalArgTypes=Nil,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedStorageLocType = vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.ListElementType),
          returnType=vt.ReturnType.SingleValue(vt.SymbolType)
        )

        val expectedLambdaType = vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, _, _, _, _, _))) =>
        val expectedType = vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
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
      case et.Lambda(polyType, Nil, Nil, None, body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(exprFor("(lambda (x) x)")) {
      case et.Lambda(polyType, List(argX), Nil, None, body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(argX.schemeType === vt.AnySchemeType)
        assert(body === et.VarRef(argX))
    }

    inside(exprFor("(lambda x x)")) {
      case et.Lambda(polyType, Nil, Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda (x y . z) x y z)")) {
      case et.Lambda(polyType, List(argX, argY), Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnySchemeType, vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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

  test("untyped optionals lambdas") {
    val expectedDefault = et.Cond(
      et.Literal(ast.BooleanLiteral(true)),
      et.Literal(ast.IntegerLiteral(1)),
      et.Literal(ast.IntegerLiteral(2))
    )

    inside(exprFor("(lambda ([x (if #t 1 2)]) #t)")) {
      case et.Lambda(polyType, Nil, List(et.OptionalArg(argX, `expectedDefault`)), None, body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(argX.schemeType === vt.AnySchemeType)
        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }
  }

  test("typed lambdas") {
    inside(exprFor("(lambda ([x : <number>]) x)")(nfiScope)) {
      case et.Lambda(polyType, List(argX), Nil, None, body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.NumberType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(argX.schemeType === vt.NumberType)
        assert(body === et.VarRef(argX))
    }

    inside(exprFor("(lambda (x : <any> *) x)")(nfiScope)) {
      case et.Lambda(polyType, Nil, Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.AnySchemeType))

        assert(body === et.VarRef(restArg))
    }

    inside(exprFor("(lambda ([x : <exact-integer>] y z : <symbol> *) x y z)")(nfiScope)) {
      case et.Lambda(polyType, List(argX, argY), Nil, Some(restArg), body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.ExactIntegerType, vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.SymbolType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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

  test("typed optionals lambdas") {
    val expectedDefault = et.Literal(ast.StringLiteral("default"))

    inside(exprFor("(lambda ([x : <string> \"default\"]) #t)")(nfiScope)) {
      case et.Lambda(polyType, Nil, List(et.OptionalArg(argX, `expectedDefault`)), None, body, _) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(argX.schemeType === vt.StringType)
        assert(body === et.Literal(ast.BooleanLiteral(true)))
    }
  }

  test("self-executing lambdas") {
    inside(exprFor("((lambda (x) x) 1)")) {
      case et.Apply(et.Lambda(polyType, List(argX), Nil, None, body, _), List(value)) =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.Lambda(_, List(xLoc), Nil, None, et.InternalDefine(
        List(
          et.SingleBinding(fooLoc, et.Lambda(_, List(yLoc), Nil, None, fooExpr, _)),
          et.SingleBinding(barLoc, et.Lambda(_, List(aLoc, bLoc), Nil, None, barExpr, _))
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Nil, None, bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Nil, None, bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.AnySchemeType)
        assert(bodyExpr === et.Literal(ast.BooleanLiteral(true)))
    }

    inside(scope.get("return-true").value) {
      case storageLoc : StorageLocation =>
        assert(storageLoc.sourceName === "return-true")
    }
  }

  test("untyped optional arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-true [unused-param #t]) #t)")(scope)
    val procLoc = scope.get("return-true").value

    val expectedDefault = et.Literal(ast.BooleanLiteral(true))

    inside(expr) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc,
          et.Lambda(polyType, Nil, List(et.OptionalArg(fixedArg, expectedDefault)), None, bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Nil, None, _, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.SymbolType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.SymbolType)
    }
  }

  test("untyped fixed and rest arg lambda shorthand") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-false some . rest) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Nil, Some(restArg), bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.AnySchemeType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Nil, Some(restArg), _, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.BooleanType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(fixedArg.schemeType === vt.BooleanType)
        assert(restArg.schemeType === vt.UniformProperListType(vt.StringType))
    }
  }

  test("typed rest arg lambda shorthand with type constructor") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val expr = exprFor("(define (return-false rest : (Listof <string>) *) #f)")(scope)
    val procLoc = scope.get("return-false").value

    inside(expr) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Nil, Some(restArg), _, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.UniformProperListType(vt.StringType)),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.UniformProperListType(vt.StringType)))
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, List(fixedArg), Nil, Some(restArg), _, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=List(vt.BooleanType),
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Nil, Some(restArg), bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
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
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Nil, Some(restArg), _, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.PortType),
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(restArg.schemeType === vt.UniformProperListType(vt.PortType))
    }
  }

  test("recursive lambda") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor("(define (return-self) return-self)")(scope)
    val procLoc = scope.get("return-self").value
    inside(expr) {
      case et.TopLevelDefine(et.SingleBinding(storageLoc, et.Lambda(polyType, Nil, Nil, None, bodyExpr, _)))
          if procLoc == storageLoc =>
        assert(polyType === vt.ProcedureType(
          mandatoryArgTypes=Nil,
          optionalArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.AnySchemeType)
        ).toPolymorphic)

        assert(bodyExpr === et.VarRef(storageLoc))
    }
  }

  test("duplicate formals failure") {
    intercept[BadSpecialFormException] {
      exprFor("(lambda (x x) x)")
    }
  }

  test("duplicate body variable failure") {
    intercept[DuplicateDefinitionException] {
      exprFor(
        """(lambda ()
             (define-type <type> <symbol>)
             (define-type <type> <flonum>))"""
      )(nfiScope)
    }
  }

  test("annotating variable with record type introduced in same body") {
    val expr = exprFor(
      """(lambda ()
           (define-record-type <rec> (rec) rec?)
           (define val : <rec> (rec)))"""
    )
  }
}
