package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{RangeException, TypeException}
import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class ListProcSuite extends FunSuite with PlanHelpers {
  test("static predicates") {
    assertStaticPlan("(null? '())",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(null? '(1 . 2))",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(pair? '())",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(pair? '(1 . 2))",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""
      (define-type <number-symbol-pair> (Pairof <number> <symbol>))
      ((make-predicate <number-symbol-pair>) '(1 . hello))
      """, ast.BooleanLiteral(true))
    
    assertStaticPlan("""
      (define-type <number-symbol-pair> (Pairof <number> <symbol>))
      ((make-predicate <number-symbol-pair>) '(hello . 1))
      """, ast.BooleanLiteral(false))
    
    // List of strings is a list of strings
    assertStaticPlan("""
      (define-type <string-list> (Listof <string>))
      ((make-predicate <string-list>) '("one" "two" "three"))
      """, ast.BooleanLiteral(true))
    
    // Empty list is a list of strings
    assertStaticPlan("""
      (define-type <string-list> (Listof <string>))
      ((make-predicate <string-list>) '())
      """, ast.BooleanLiteral(true))
    
    // List containing symbol is not a list of strings
    assertStaticPlan("""
      (define-type <string-list> (Listof <string>))
      ((make-predicate <string-list>) '("one" 'two "three"))
      """, ast.BooleanLiteral(false))
    
    // List-of-list-of-strings is a list-of-list-of-strings
    assertStaticPlan("""
      (define-type <string-list-list> (Listof (Listof <string>)))
      ((make-predicate <string-list-list>) '(("one" "two") ("three") ()))
      """, ast.BooleanLiteral(true))
    
    // List of strings is not a list-of-list-of-strings
    assertStaticPlan("""
      (define-type <string-list-list> (Listof (Listof <string>)))
      ((make-predicate <string-list-list>) '("one" "two" "three"))
      """, ast.BooleanLiteral(false))
  }

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

  test("static (list)") {
    assertStaticPlan("(null? (list))",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(length (list))",
      ast.IntegerLiteral(0)
    )
    
    assertStaticPlan("(length (list 1 2 3 4 5))",
      ast.IntegerLiteral(5)
    )
  }
  
  test("static (cons)") {
    assertStaticPlan("(cons 1 2)",
      ast.Pair(
        ast.IntegerLiteral(1),
        ast.IntegerLiteral(2)
      )
    )
  }
  
  test("static (append)") {
    assertStaticPlan("(append)",
      ast.EmptyList()
    )
    
    assertStaticPlan("(append #t)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(append '(1 2) '(3 4))",
      ast.ProperList(List(
        ast.IntegerLiteral(1),
        ast.IntegerLiteral(2),
        ast.IntegerLiteral(3),
        ast.IntegerLiteral(4)
      ))
    )
    
    assertStaticPlan("(append '(1 2 3) 4)",
      ast.AnyList(List(
        ast.IntegerLiteral(1),
        ast.IntegerLiteral(2),
        ast.IntegerLiteral(3)
      ), ast.IntegerLiteral(4))
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
  
  test("static (list-tail)") {
    assertStaticPlan("(list-tail '(1 2 3) 1)",
      ast.ProperList(List(
        ast.IntegerLiteral(2),
        ast.IntegerLiteral(3)
      ))
    )
    
    assertStaticPlan("(list-tail '(1 2 3) 3)",
      ast.EmptyList()
    )

    intercept[RangeException] {
      planStepsFor("(list-tail '(1 2 3) 4)")
    }
  }

  test("static (list-ref)") {
    assertStaticPlan("(list-ref '(1 2 3) 0)",
      ast.IntegerLiteral(1)
    )

    assertStaticPlan("(list-ref '(1 2 3) 2)",
      ast.IntegerLiteral(3)
    )

    intercept[TypeException] {
      planStepsFor("(list-ref '(1 2 3) 3)")
    }
  }
}
