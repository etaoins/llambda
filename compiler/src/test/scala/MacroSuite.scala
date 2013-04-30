package test.scala

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class MacroSuite extends FunSuite with Inside with OptionValues with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("multiple template is error") {
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax six
             (syntax-rules ()
             ((six)
               5
               6
           )))""")

    }
  }

  test("trivial replacement") {
    assert(expressionFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((false-literal)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.FalseLiteral))
  }
  
  test("first pattern symbol is ignored") {
    assert(expressionFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((SOMETHING-COMPLETELY-DIFFERENT)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.FalseLiteral))
  }
  
  test("simple expansion") {
    assert(expressionFor(
      """(define-syntax return-single
           (syntax-rules ()
             ((return-single foo)
               foo
         )))
         (return-single 6)"""
    ) === et.Literal(ast.IntegerLiteral(6)))
  }
  
  test("two value expansion") {
    assert(expressionFor(
      """(define-syntax return-two
           (syntax-rules ()
             ((return-two a b)
               '(a . b)
         )))
         (return-two #t #f)"""
    ) === et.Literal(ast.Pair(ast.TrueLiteral, ast.FalseLiteral)))
  }
  
  test("literals must exactly match") {
    intercept[NoSyntaxRuleException] {
      expressionFor(
        """(define-syntax for
             (syntax-rules (in)
               ((for a in b)
                 '(a . b)
           )))
           (for 1 foo 2)""")
    }
  }
  
  test("multiple rules") {
    assert(expressionFor(
      """(define-syntax arg-count
           (syntax-rules ()
             ((arg-count) 0)
             ((arg-count _) 1)
             ((arg-count _ _) 2)
         ))
         (arg-count 1.0)"""
    ) === et.Literal(ast.IntegerLiteral(1)))
  }
  
  test("recursive expansion") {
    assert(expressionFor(
      """(define-syntax recurse
           (syntax-rules ()
             ((recurse) 7)
             ((recurse _) (recurse))
         ))
         (recurse 'a)"""
    ) === et.Literal(ast.IntegerLiteral(7)))
  }
  
  test("wildcards") {
    assert(expressionFor(
      """(define-syntax return-second
           (syntax-rules ()
             ((return-second _ foo)
               foo
         )))
         (return-second 'a 'b)"""
    ) === et.Literal(ast.Symbol("b")))
  }

  test("zero or more match") {
    assert(expressionFor(
      """(define-syntax return-all
           (syntax-rules ()
             ((return-all values ...)
               '(values ...)
         )))
         (return-all 1 2 3)"""
    ) === et.Literal(ast.ProperList(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.IntegerLiteral(3))))
  }
}

