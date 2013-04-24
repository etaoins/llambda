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
    assert(expressionFor("a") === et.VarReference(et.UnresolvedVar("a")))
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
      et.UnresolvedVar("+"),
      List(
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(3)),
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(4))
      )
    ))

    // R7RS explicitly calls this an error
    intercept[MalformedExpressionException] {
      expressionFor("()")
    }
  }

  test("lambdas") {
    assert(expressionFor("(lambda (x) (+ x x))") === et.Procedure(
      List(et.UnresolvedVar("x")), None,
      List(
        et.Application(
          et.UnresolvedVar("+"), 
          List(
            et.VarReference(et.UnresolvedVar("x")),
            et.VarReference(et.UnresolvedVar("x"))
          )
        )
      )
    ))

    assert(expressionFor("(lambda x x)") === et.Procedure(
      Nil, Some(et.UnresolvedVar("x")),
      List(et.VarReference(et.UnresolvedVar("x")))
    ))

    assert(expressionFor("(lambda (x y . z) z)") === et.Procedure(
      List(et.UnresolvedVar("x"), et.UnresolvedVar("y")), Some(et.UnresolvedVar("z")),
      List(et.VarReference(et.UnresolvedVar("z")))
    ))
  }

  test("conditionals") {
    assert(expressionFor("(if (> 3 2) 'yes 'no)") === et.Conditional(
      et.Application(et.UnresolvedVar(">"), List(
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(3)),
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(2)))
      ),
      et.Literal(ast.Symbol("yes")),
      Some(et.Literal[et.UnresolvedVar](ast.Symbol("no")))
    ))
    
    assert(expressionFor("(if (> 3 2) 'yes)") === et.Conditional(
      et.Application(et.UnresolvedVar(">"), List(
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(3)),
        et.Literal[et.UnresolvedVar](ast.IntegerLiteral(2)))),
      et.Literal(ast.Symbol("yes")),
      None
    ))
  }

  test("define") {
    assert(expressionFor("""(define a 2)""") === et.DefineVar(
      et.UnresolvedVar("a"), et.Literal(ast.IntegerLiteral(2))
    ))

    assert(expressionFor("""(define (return-true unused-param) #t)""") === et.DefineVar(
      et.UnresolvedVar("return-true"),
      et.Procedure(
        List(et.UnresolvedVar("unused-param")),
        None,
        List(et.Literal[et.UnresolvedVar](ast.TrueLiteral))
      )
    ))
    
    assert(expressionFor("""(define (return-false some . rest) #f)""") === et.DefineVar(
      et.UnresolvedVar("return-false"),
      et.Procedure(
        List(et.UnresolvedVar("some")),
        Some(et.UnresolvedVar("rest")),
        List(et.Literal[et.UnresolvedVar](ast.FalseLiteral))
      )
    ))
    
    assert(expressionFor("""(define (return-six . rest) 6)""") === et.DefineVar(
      et.UnresolvedVar("return-six"),
      et.Procedure(
        List(),
        Some(et.UnresolvedVar("rest")),
        List(et.Literal[et.UnresolvedVar](ast.IntegerLiteral(6)))
      )
    ))
  }

  test("macros") {
    assert(expressionFor(
      """(define-syntax and
           (syntax-rules ()
             ((and) #t)
             ((and test) test)
             ((and test1 test2 ...)
               (if test1 (and test2 ...) #f))))"""
      ) === et.DefineSyntax[et.UnresolvedVar](et.UnresolvedVar("and"), Nil, List(
        et.SyntaxRule(Nil, et.Literal(ast.TrueLiteral)),
        et.SyntaxRule(List(ast.Symbol("test")), et.VarReference(et.UnresolvedVar("test"))),
        et.SyntaxRule(List(ast.Symbol("test1"), ast.Symbol("test2"), ast.Symbol("...")), 
          et.Conditional(
            et.VarReference(et.UnresolvedVar("test1")),
            et.Application(et.UnresolvedVar("and"),
              List(et.VarReference(et.UnresolvedVar("test2")), et.VarReference(et.UnresolvedVar("...")))
            ),
            Some(et.Literal(ast.FalseLiteral))
          )
        )
      )))

    assert(expressionFor(
      """(define-syntax cond
           (syntax-rules (else =>)
             ((cond (else result1 result2 ...))
              (begin result1 result2 ...))))"""
    ) === et.DefineSyntax[et.UnresolvedVar](et.UnresolvedVar("cond"), List("else", "=>"),
      List(
        et.SyntaxRule(
          List(ast.ProperList(
              ast.Symbol("else"), ast.Symbol("result1"), ast.Symbol("result2"), ast.Symbol("...")
          )),
          et.Application(et.UnresolvedVar("begin"), 
            List(
              et.VarReference(et.UnresolvedVar("result1")),
              et.VarReference(et.UnresolvedVar("result2")),
              et.VarReference(et.UnresolvedVar("..."))
            )
          )
        )
      )
    ))
  }
}

