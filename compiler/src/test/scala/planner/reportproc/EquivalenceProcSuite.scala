package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class EquivalenceProcSuite extends FunSuite with PlanHelpers {
  test("boolean (eqv?)") {
    assertStaticPlan("(eqv? #t #t)",  
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? #f #f)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? #f #t)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(eqv? #f '())",
      ast.BooleanLiteral(false)
    )
  }
  
  test("symbol (eqv?)") {
    assertStaticPlan("(eqv? 'one 'one)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? 'one 'two)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(eqv? 'one '())",
      ast.BooleanLiteral(false)
    )
  }
  
  // This isn't defined by R7RS but our runtime does it to be nice
  test("string (eqv?)") {
    assertStaticPlan("""(eqv? "one" "one")""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(eqv? "one" "two")""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(eqv? "one" '())""",
      ast.BooleanLiteral(false)
    )
  }
  
  test("exact int (eqv?)") {
    assertStaticPlan("(eqv? 5 5)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? -5 5)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(eqv? -5 '())",
      ast.BooleanLiteral(false)
    )
  }
  
  test("inexact rational (eqv?)") {
    assertStaticPlan("(eqv? 5.6 5.6)",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? -50.0 50.0)",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("(eqv? -50.0 '())",
      ast.BooleanLiteral(false)
    )
  }
  
  test("character (eqv?)") {
    assertStaticPlan("""(eqv? #\backspace #\backspace)""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(eqv? #\backspace #\delete)""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(eqv? #\backspace '())""",
      ast.BooleanLiteral(false)
    )
  }

  test("empty list (eqv?)") {
    assertStaticPlan("(eqv? '() '())",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("(eqv? '() #f)",
      ast.BooleanLiteral(false)
    )
  }
  
  test("list (equal?)") {
    assertStaticPlan("""(equal? '(1 2 (3 4)) '(1 2 (3 4)))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? '(1 2 (3 4)) '(1 2 (3 4 5)))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? '(1 2 3 4) '(1 2 3 4))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? '(1 2 3 4) '(1 2 3 4 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? '(1 2 3 . 4) '(1 2 3 . 4))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? '(1 2 3 . 4) '(1 2 3 . 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? '(1 2 3 4) #f)""",
      ast.BooleanLiteral(false)
    )
  }
  
  test("vector (equal?)") {
    assertStaticPlan("""(equal? #(1 2 (3 4)) #(1 2 (3 4)))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? #(1 2 (3 4)) #(1 2 (3 4 5)))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 4) #(1 2 3 4))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 4) #(1 2 3 4 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 4) #(1 2 3 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 #(4 5)) #(1 2 3 #(4 5)))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 #(4 5)) '(1 2 3 #(4 6)))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #(1 2 3 4) #f)""",
      ast.BooleanLiteral(false)
    )
  }
  
  test("bytevector (equal?)") {
    assertStaticPlan("""(equal? #u8(1 2 3 4) #u8(1 2 3 4))""",
      ast.BooleanLiteral(true)
    )
    
    assertStaticPlan("""(equal? #u8(1 2 3 4) #u8(1 2 3 4 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #u8(1 2 3 4) #u8(1 2 3 5))""",
      ast.BooleanLiteral(false)
    )
    
    assertStaticPlan("""(equal? #u8(1 2 3 4) #f)""",
      ast.BooleanLiteral(false)
    )
  }
}
