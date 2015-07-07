package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite

class ConvertCurlyInfixExprSuite extends FunSuite {
  test("simple c-expressions") {
    val threeElementResult = ConvertCurlyInfixExpr(List(
      ast.IntegerLiteral(1),
      ast.Symbol("+"),
      ast.IntegerLiteral(2)
    ))

    assert(threeElementResult === ast.ProperList(List(
      ast.Symbol("+"),
      ast.IntegerLiteral(1),
      ast.IntegerLiteral(2)
    )))

    val fiveElementResult = ConvertCurlyInfixExpr(List(
      ast.IntegerLiteral(1),
      ast.Symbol("+"),
      ast.IntegerLiteral(2),
      ast.Symbol("+"),
      ast.IntegerLiteral(3)
    ))

    assert(fiveElementResult === ast.ProperList(List(
      ast.Symbol("+"),
      ast.IntegerLiteral(1),
      ast.IntegerLiteral(2),
      ast.IntegerLiteral(3)
    )))

    intercept[InvalidCurlyInfixExprException] {
      // Mismatched even datum
      ConvertCurlyInfixExpr(List(
        ast.IntegerLiteral(1),
        ast.Symbol("+"),
        ast.IntegerLiteral(2),
        ast.Symbol("-"),
        ast.IntegerLiteral(3)
      ))
    }
  }

  test("empty c-expressions") {
    val result = ConvertCurlyInfixExpr(Nil)

    assert(result === ast.EmptyList())
  }

  test("escaping c-expressions") {
    val result = ConvertCurlyInfixExpr(List(ast.IntegerLiteral(5)))

    assert(result === ast.IntegerLiteral(5))
  }

  test("unary c-expressions") {
    val result = ConvertCurlyInfixExpr(List(
      ast.Symbol("-"),
      ast.Symbol("x")
    ))

    assert(result === ast.ProperList(List(
      ast.Symbol("-"),
      ast.Symbol("x")
    )))
  }

  test("four element c-expression fails") {
    intercept[InvalidCurlyInfixExprException] {
      ConvertCurlyInfixExpr(List(
        ast.IntegerLiteral(1),
        ast.Symbol("+"),
        ast.IntegerLiteral(2),
        ast.Symbol("+")
      ))
    }
  }
}
