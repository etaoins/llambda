package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class ListProcSuite extends FunSuite with PlanHelpers {
  test("static (length)") {
    assertStaticPlan("(length '())",
      ast.IntegerLiteral(0)
    )
    
    assertStaticPlan("(length '(1))",
      ast.IntegerLiteral(1)
    )
    
    assertStaticPlan("(length '(1 2 3))",
      ast.IntegerLiteral(3)
    )
  }

  test("static cadr") {
    assertStaticPlan("(car '(a b c))",
      ast.Symbol("a")
    )
    
    assertStaticPlan("(cdr '(a b c))",
      ast.ProperList(List(
        ast.Symbol("b"),
        ast.Symbol("c")
      ))
    )
  }
  
  test("static (memq)") {
    assertStaticPlan("(memq 'a '(a b c))",
      ast.ProperList(List(
        ast.Symbol("a"),
        ast.Symbol("b"),
        ast.Symbol("c")
      ))
    )
    
    assertStaticPlan("(memq 'b '(a b c))",
      ast.ProperList(List(
        ast.Symbol("b"),
        ast.Symbol("c")
      ))
    )
    
    assertStaticPlan("(memq 'a '(b c d))",
      ast.BooleanLiteral(false)
    )
  }

  test("static (member)") {
    assertStaticPlan("(member '(a) '(b (a) c))",
      ast.ProperList(List(
        ast.ProperList(List(
          ast.Symbol("a")
        )),
        ast.Symbol("c")
      ))
    )
  }

  test("static (case)") {
    // This internally uses (memv)
    assertStaticPlan("""
      (case 'a
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      """, ast.Symbol("vowel")
    )
    
    assertStaticPlan("""
      (case 'c
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      """, ast.Symbol("c")
    )
  }
}
