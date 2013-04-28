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
  
  test("simple expansion") {
    assert(expressionFor(
      """(define-syntax return-single
           (syntax-rules ()
             ((freturn-single foo)
               foo
         )))
         (return-single 6)"""
    ) === et.Literal(ast.IntegerLiteral(6)))
  }
  
  test("wildcards") {
    assert(expressionFor(
      """(define-syntax return-second
           (syntax-rules ()
             ((freturn-second _ foo)
               foo
         )))
         (return-second 'a 'b)"""
    ) === et.Literal(ast.Symbol("b")))
  }
}

