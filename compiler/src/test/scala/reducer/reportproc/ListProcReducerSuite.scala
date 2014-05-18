package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class ListProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  implicit val scope = schemeBaseScope

  test("type predicates") {
    assert(reductionFor("(null? '())") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(null? #f)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(pair? '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(pair? '(1 2 3))") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(list? '())") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(list? '(1 2 3))") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(list? #f)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(list? '(1 . 2))") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (length)") {
    assert(reductionFor("(length '())") ===
      et.Literal(ast.IntegerLiteral(0))
    )
    
    assert(reductionFor("(length '(1))") ===
      et.Literal(ast.IntegerLiteral(1))
    )
    
    assert(reductionFor("(length '(1 2 3))") ===
      et.Literal(ast.IntegerLiteral(3))
    )
  }

  test("static (memq)") {
    assert(reductionFor("(memq 'a '(a b c))") ===
      et.Literal(ast.ProperList(List(
        ast.Symbol("a"),
        ast.Symbol("b"),
        ast.Symbol("c")
      )))
    )
    
    assert(reductionFor("(memq 'b '(a b c))") ===
      et.Literal(ast.ProperList(List(
        ast.Symbol("b"),
        ast.Symbol("c")
      )))
    )
    
    assert(reductionFor("(memq 'a '(b c d))") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }

  test("static (member)") {
    assert(reductionFor("(member '(a) '(b (a) c))") ===
      et.Literal(ast.ProperList(List(
        ast.ProperList(List(
          ast.Symbol("a")
        )),
        ast.Symbol("c")
      )))
    )
  }
}
