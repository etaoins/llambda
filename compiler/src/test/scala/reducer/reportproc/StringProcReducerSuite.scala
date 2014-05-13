package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class StringProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  implicit val scope = schemeBaseScope

  test("type predicates") {
    assert(reductionFor("""(string? "Hello")""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(string? #f)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (string-length)") {
    assert(reductionFor("""(string-length "Hello")""") ===
      et.Literal(ast.IntegerLiteral(5))
    )

    assert(reductionFor("""(string-length "日本国")""") ===
      et.Literal(ast.IntegerLiteral(3))
    )
    
    inside(reductionFor("""(string-length 'symbol)""")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.Symbol("symbol"))
      )) =>
        Unit
    }
  }

  test("static (string->symbol)") {
    assert(reductionFor("""(string->symbol "Hello")""") ===
      et.Literal(ast.Symbol("Hello"))
    )

    assert(reductionFor("""(string->symbol "日本国")""") ===
      et.Literal(ast.Symbol("日本国"))
    )

    // We shouldn't do this as it could cause duplicate data
    inside(bindlessReductionFor("""
      (define test-string "Hello")
      (string->symbol test-string)
    """)) {
      case et.Apply(_, _) =>
        Unit
    }
    
    inside(reductionFor("""(string->symbol 'symbol)""")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.Symbol("symbol"))
      )) =>
        Unit
    }
  }
}
