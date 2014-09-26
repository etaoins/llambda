package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._

class ExtractCaseLambdaBodySuite extends FunSuite with Inside with testutil.ExprHelpers {
  implicit val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))
  
  test("(case-lambda) with no clauses") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    val caseExpr = exprFor("""(case-lambda)""")(scope)
    assert(caseExpr === et.CaseLambda(Nil))
  }

  test("untyped (case-lambda) with fixed args") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(case-lambda
           (() #t)
           ((one) #f))"""
    )(scope)) {
      case caseExpr @ et.CaseLambda(List(firstLambda, secondLambda)) =>
        val firstProcType = vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        )
        
        val secondProcType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        )

        assert(caseExpr.schemeType === vt.CaseProcedureType(List(firstProcType, secondProcType)))

        inside(firstLambda) {
          case et.Lambda(`firstProcType`, Nil, None, et.Literal(ast.BooleanLiteral(true)), _) =>
        }
        
        inside(secondLambda) {
          case et.Lambda(`secondProcType`, List(_), None, et.Literal(ast.BooleanLiteral(false)), _) =>
        }
    }
  }
  
  test("untyped (case-lambda) with rest argument") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    inside(exprFor(
      """(case-lambda
           ((one) #t)
           ((first . rest) #f))"""
    )(scope)) {
      case caseExpr @ et.CaseLambda(List(firstLambda, secondLambda)) =>
        val firstProcType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.ArbitraryValues
        )
        
        val secondProcType = vt.ProcedureType(
          fixedArgTypes=List(vt.AnySchemeType),
          restArgMemberTypeOpt=Some(vt.AnySchemeType),
          returnType=vt.ReturnType.ArbitraryValues
        )

        assert(caseExpr.schemeType === vt.CaseProcedureType(List(firstProcType, secondProcType)))

        inside(firstLambda) {
          case et.Lambda(`firstProcType`, List(_), None, et.Literal(ast.BooleanLiteral(true)), _) =>
        }
        
        inside(secondLambda) {
          case et.Lambda(`secondProcType`, List(_), Some(_), et.Literal(ast.BooleanLiteral(false)), _) =>
        }
    }
  }
  
  test("(case-lambda) clause with same arity fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(case-lambda
             ((one) #t)
             ((one) #f))"""
      )(scope)
    }
  }
  
  test("(case-lambda) clause with after rest arg fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      exprFor(
        """(case-lambda
             ((one . rest) #t)
             ((one two) #f))"""
      )(scope)
    }
  }
}
