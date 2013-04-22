package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class ExtractExpressionsSuite extends FunSuite with Inside {
  private def expressionFor(scheme : String) =
    SchemeParser(scheme) match {
      case SchemeParser.Success(datum :: Nil, _) =>
        ExtractExpressions(datum)
      case err =>
        fail(err.toString)
    }

  test("variable reference") {
    assert(expressionFor("a") === et.VarReference("a"))
  }

  test("literals") {
    assert(expressionFor("'a") === et.Literal(ast.Symbol("a")))

    assert(expressionFor("'#(a b c)") === et.Literal(
      ast.Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c"))
    ))
    
    assert(expressionFor("'()") === et.Literal(ast.EmptyList))
    
    assert(expressionFor("'(+ 1 2)") === et.Literal(
      ast.ProperList(ast.Symbol("+"), ast.IntegerLiteral(1), ast.IntegerLiteral(2))
    ))
    
    assert(expressionFor("'(quote a)") === et.Literal(
      ast.ProperList(ast.Symbol("quote"), ast.Symbol("a"))
    ))
    
    assert(expressionFor("''a") === et.Literal(
      ast.ProperList(ast.Symbol("quote"), ast.Symbol("a"))
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
      ast.Vector(ast.Symbol("a"), ast.IntegerLiteral(10))
    ))
    
    assert(expressionFor("#(a 10)") === et.Literal(
      ast.Vector(ast.Symbol("a"), ast.IntegerLiteral(10))
    ))
    
    assert(expressionFor("'#u8(64 65)") === et.Literal(
      ast.ByteVector(64, 65)
    ))
    
    assert(expressionFor("#u8(64 65)") === et.Literal(
      ast.ByteVector(64, 65)
    ))
    
    assert(expressionFor("'#t") === et.Literal(
      ast.TrueLiteral
    ))
    
    assert(expressionFor("#t") === et.Literal(
      ast.TrueLiteral
    ))
  }

  test("application") {
    assert(expressionFor("(+ 3 4)") === et.Application(
      "+",
      List(et.Literal(ast.IntegerLiteral(3)), et.Literal(ast.IntegerLiteral(4)))
    ))

    // R7RS explicitly calls this an error
    intercept[MalformedExpressionException] {
      expressionFor("()")
    }
  }

  test("lambdas") {
    assert(expressionFor("(lambda (x) (+ x x))") === et.Procedure(
      List("x"), None,
      List(
        et.Application("+", List(et.VarReference("x"), et.VarReference("x")))
      )
    ))

    assert(expressionFor("(lambda x x)") === et.Procedure(
      Nil, Some("x"),
      List(et.VarReference("x"))
    ))

    assert(expressionFor("(lambda (x y . z) z)") === et.Procedure(
      List("x", "y"), Some("z"),
      List(et.VarReference("z"))
    ))
  }

  test("conditionals") {
    assert(expressionFor("(if (> 3 2) 'yes 'no)") === et.Conditional(
      et.Application(">", List(et.Literal(ast.IntegerLiteral(3)), et.Literal(ast.IntegerLiteral(2)))),
      et.Literal(ast.Symbol("yes")),
      Some(et.Literal(ast.Symbol("no")))
    ))
    
    assert(expressionFor("(if (> 3 2) 'yes)") === et.Conditional(
      et.Application(">", List(et.Literal(ast.IntegerLiteral(3)), et.Literal(ast.IntegerLiteral(2)))),
      et.Literal(ast.Symbol("yes")),
      None
    ))
  }
}

