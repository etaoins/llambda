package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class EquivalenceProcReducerSuite extends FunSuite with testutil.ExpressionHelpers {
  val schemeBaseBindings = libraryLoader.loadSchemeBase(frontendConfig)
  implicit val scope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

  test("boolean (eqv?)") {
    assert(reductionFor("(eqv? #t #t)") ===  
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? #f #f)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? #f #t)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(eqv? #f '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("symbol (eqv?)") {
    assert(reductionFor("(eqv? 'one 'one)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? 'one 'two)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(eqv? 'one '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  // This isn't defined by R7RS but our runtime does it to be nice
  test("string (eqv?)") {
    assert(reductionFor("""(eqv? "one" "one")""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(eqv? "one" "two")""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(eqv? "one" '())""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("exact int (eqv?)") {
    assert(reductionFor("(eqv? 5 5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? -5 5)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(eqv? -5 '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("inexact rational (eqv?)") {
    assert(reductionFor("(eqv? 5.6 5.6)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? -50.0 50.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(eqv? -50.0 '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("character (eqv?)") {
    assert(reductionFor("""(eqv? #\backspace #\backspace)""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(eqv? #\backspace #\delete)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(eqv? #\backspace '())""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("unit (eqv?)") {
    assert(reductionFor("""(eqv? #!unit #!unit)""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(eqv? #!unit '())""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("empty list (eqv?)") {
    assert(reductionFor("(eqv? '() '())") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(eqv? '() #f)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("list (equal?)") {
    assert(reductionFor("""(equal? '(1 2 (3 4)) '(1 2 (3 4)))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? '(1 2 (3 4)) '(1 2 (3 4 5)))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? '(1 2 3 4) '(1 2 3 4))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? '(1 2 3 4) '(1 2 3 4 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? '(1 2 3 . 4) '(1 2 3 . 4))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? '(1 2 3 . 4) '(1 2 3 . 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? '(1 2 3 4) #f)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("vector (equal?)") {
    assert(reductionFor("""(equal? #(1 2 (3 4)) #(1 2 (3 4)))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? #(1 2 (3 4)) #(1 2 (3 4 5)))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 4) #(1 2 3 4))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 4) #(1 2 3 4 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 4) #(1 2 3 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 #(4 5)) #(1 2 3 #(4 5)))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 #(4 5)) '(1 2 3 #(4 6)))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #(1 2 3 4) #f)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("bytevector (equal?)") {
    assert(reductionFor("""(equal? #u8(1 2 3 4) #u8(1 2 3 4))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(equal? #u8(1 2 3 4) #u8(1 2 3 4 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #u8(1 2 3 4) #u8(1 2 3 5))""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("""(equal? #u8(1 2 3 4) #f)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
}
