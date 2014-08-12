package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.SchemeStringImplicits._

import llambda.compiler._
import org.scalatest.FunSuite

class RetypeLambdaArgsSuite extends FunSuite with PlanHelpers{
  private def retypingProcedureName = "retyping-procedure"

  private def signatureFor(scheme : String) : ProcedureSignature = {
    val importDecl = datum"(import (scheme base) (llambda typed) (llambda test-util))"

    // Give the procedure a distinctive name so we can find it later
    val procedureDatum = ast.ProperList(List(
      ast.Symbol("define"),
      ast.Symbol(retypingProcedureName),
      SchemeParser.parseStringAsData(scheme, None).head
    ))
    
    // Reference the procedure to force it to be planned
    // Higher optimization levels would see right through this but this works on -O 0
    val referenceDatum = ast.ProperList(List(
      ast.Symbol("procedure?"),
      ast.Symbol(retypingProcedureName)
    ))

    val data = List(importDecl, procedureDatum, referenceDatum)

    val functions = planForData(data, optimise=false)
    functions(retypingProcedureName).signature
  }

  test("argless procedure returning integer constant") {
    val signature = signatureFor("""(lambda () 1)""")

    assert(signature.hasWorldArg === false)
    assert(signature.fixedArgs === Nil)
    assert(signature.returnType === Some(vt.Int64))
  }
  
  test("procedure returning its argument") {
    val signature = signatureFor("""(lambda (x) x)""")

    // This can be passed anything so it can return anything
    assert(signature.hasWorldArg === false)
    assert(signature.fixedArgs === List(vt.AnySchemeType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("explicitly typed procedure returning its argument") {
    val signature = signatureFor("""(lambda: ((x : <integer>)) x)""")

    assert(signature.hasWorldArg === false)
    assert(signature.fixedArgs === List(vt.ExactIntegerType))
    assert(signature.returnType === Some(vt.ExactIntegerType))
  }

  test("explicitly casting procedure argument to type") {
    val signature = signatureFor("""(lambda (x) (cast x <char>))""")

    assert(signature.fixedArgs === List(vt.CharacterType))
    assert(signature.returnType === Some(vt.CharacterType))
  }

  test("assigning procedure argument to typed immutable") {
    val signature = signatureFor("""
      (lambda (x)
        (define: y : <flonum> x)
        y)"""
    )

    assert(signature.fixedArgs === List(vt.InexactRationalType))
    assert(signature.returnType === Some(vt.InexactRationalType))
  }
  
  test("assigning procedure argument to typed mutable") {
    val signature = signatureFor("""
      (lambda (x)
        (define: y : <flonum> x)
        (set! y 5.0)
        y)"""
    )

    assert(signature.fixedArgs === List(vt.InexactRationalType))
  }
  
  test("procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda (vec index) (vector-ref vec index))""")

    // This can be passed anything so it can return anything
    // Note that this could really be ct.UInt32 but we're not smart enough to figure that out
    assert(signature.fixedArgs === List(vt.VectorType, vt.ExactIntegerType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("typed procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda: ((vec : <vector>) (index : <number>)) (vector-ref vec index))""")

    // We should refine <number> in to <integer>
    assert(signature.fixedArgs === List(vt.VectorType, vt.ExactIntegerType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("custom union typed procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda: ((vec : (U <vector> <char>)) (index : <number>)) (vector-ref vec index))""")

    // We should refine <number> in to <integer>
    assert(signature.fixedArgs === List(vt.VectorType, vt.ExactIntegerType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("procedure proxying (vector-set!)") {
    val signature = signatureFor("""(lambda (vec index) (vector-set! vec index #f))""")

    assert(signature.fixedArgs === List(vt.VectorType, vt.ExactIntegerType))
    assert(signature.returnType === None)
  }

  test("types used across (if) branches are unioned together") {
    val signature = signatureFor("""
      (lambda (value)
        (if dynamic-true
          (cast value <string>)
          (cast value <symbol>)))""")

    assert(signature.fixedArgs === List(vt.UnionType(Set(vt.StringType, vt.SymbolType))))
    assert(signature.returnType === Some(vt.UnionType(Set(vt.StringType, vt.SymbolType))))
  }
  
  test("procedure proxying (vector-ref) past a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if #t 1 2)
        (vector-ref vec index))""")

    // Because the (vector-ref) is unconditionally executed the condition
    // should change the conditional
    assert(signature.fixedArgs === List(vt.VectorType, vt.ExactIntegerType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }

  test("procedure proxying function with typed rest arg") {
    val signature = signatureFor("""
      (lambda (val1 val2) 
        (+ 5 val1 val2))""")

    assert(signature.fixedArgs === List(vt.NumericType, vt.NumericType))
    assert(signature.returnType === Some(vt.NumericType))
  }
  
  test("procedure proxying (vector-ref) past possible exception point") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (/ 0 0)
        (vector-ref vec index))""")

    // (/) can can throw an exception
    // This mean (vector-ref) may not be executed
    assert(signature.fixedArgs === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("procedure proxying (vector-ref) past possible exception point in true branch") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if #t (/ 0 0) #f)
        (vector-ref vec index))""")

    assert(signature.fixedArgs === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  test("procedure proxying (vector-ref) past possible exception point in false branch") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if #t #f (/ 0 0))
        (vector-ref vec index))""")

    assert(signature.fixedArgs === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }

  test("procedure proxying (>) inside a conditional test") {
    // This makes sure the types from (+) are passed down to the branches
    // (>) is used here because it can't throw exceptions
    val signature = signatureFor("""
      (lambda (m n) 
        (if (> 0 m n) #t #f))""")

    assert(signature.fixedArgs === List(vt.NumericType, vt.NumericType))
    assert(signature.returnType === Some(vt.BooleanType))
  }

  test("aborted retyping preserves original argument types") {
    val signature = signatureFor("""
      (lambda: ((vec : <vector>) (index : <number>)) 
        (/ 0 0)
        (vector-ref vec index))""")

    assert(signature.fixedArgs === List(vt.VectorType, vt.NumericType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
  
  
  test("procedure proxying (vector-ref) inside a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if (vector? vec)
          (vector-ref vec index)))""")

    // The (vector-ref) is conditionally executed, we can't assert anything about our parameters
    assert(signature.fixedArgs === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === Some(vt.AnySchemeType))
  }
}
