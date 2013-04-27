package test.scala

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class MacroSuite extends FunSuite with Inside with OptionValues with ExpressionHelpers {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  test("define simple macro") {
    assert(bindingFor(
      """(define-syntax and
           (syntax-rules ()
             ((and) #t)
             ((and test) test)
             ((and test1 test2 ...)
               (if test1 (and test2 ...) #f))))""", 
      "and"
      ) === BoundSyntax(Nil,
        List(
          SyntaxRule(Nil, ast.TrueLiteral),
          SyntaxRule(List(ast.Symbol("test")), ast.Symbol("test")),
          SyntaxRule(List(ast.Symbol("test1"), ast.Symbol("test2"), ast.Symbol("...")), 
            ast.ProperList(
              ast.Symbol("if"),
              ast.Symbol("test1"),
              ast.ProperList(ast.Symbol("and"), ast.Symbol("test2"), ast.Symbol("...")),
              ast.FalseLiteral
            )
          )
        ), primitiveScope
    ))

    assert(bindingFor(
      """(define-syntax cond
           (syntax-rules (else =>)
             ((cond (else result1 result2 ...))
               (begin result1 result2 ...))))""",
      "cond"
      ) === BoundSyntax(List("else", "=>"),
        List(
          SyntaxRule(
            List(ast.ProperList(
                ast.Symbol("else"), ast.Symbol("result1"), ast.Symbol("result2"), ast.Symbol("...")
            )),
            ast.ProperList(
              ast.Symbol("begin"), 
              ast.Symbol("result1"),
              ast.Symbol("result2"),
              ast.Symbol("...")
            )
          )
        ), primitiveScope
    ))
  }
}

