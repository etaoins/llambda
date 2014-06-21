package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.SchemeStringImplicits._

import llambda.compiler._
import org.scalatest.FunSuite

class InferArgumentTypesSuite extends FunSuite with PlanHelpers{
  private def inferringProcedureName = "inferring-procedure"

  private def signatureFor(scheme : String) : ProcedureSignature = {
    val importDecl = datum"(import (scheme base) (llambda typed))"

    // Give the procedure a distinctive name so we can find it later
    val procedureDatum = ast.ProperList(List(
      ast.Symbol("define"),
      ast.Symbol(inferringProcedureName),
      SchemeParser.parseStringAsData(scheme, None).head
    ))
    
    // Reference the procedure to force it to be planned
    // Higher optimization levels would see right through this but this works on -O 0
    val referenceDatum = ast.ProperList(List(
      ast.Symbol("procedure?"),
      ast.Symbol(inferringProcedureName)
    ))

    val data = List(importDecl, procedureDatum, referenceDatum)

    val functions = planForData(data, optimise=true, reduce=false)
    functions(inferringProcedureName).signature
  }

  test("argless procedure returning integer constant") {
    val signature = signatureFor("""(lambda () 1)""")

    assert(signature.fixedArgs === Nil)
    assert(signature.returnType === Some(vt.Int64))
  }
  
  test("procedure returning its argument") {
    val signature = signatureFor("""(lambda (x) x)""")

    // This can be passed anything so it can return anything
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.DatumCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("explicitly typed procedure returning its argument") {
    val signature = signatureFor("""(lambda: ((x : <integer>)) x)""")

    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.ExactIntegerCell)))
  }
  
  test("procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda (vec index) (vector-ref vec index))""")

    // This can be passed anything so it can return anything
    // Note that this could really be ct.UInt32 but we're not smart enough to figure that out
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("typed procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda: ((vec : <vector>) (index : <number>)) (vector-ref vec index))""")

    // We should refine <number> in to <integer>
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("procedure proxying (vector-set!)") {
    val signature = signatureFor("""(lambda (vec index) (vector-set! vec index #f))""")

    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === None)
  }
  
  test("procedure proxying (vector-ref) past a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if #t 1 2)
        (vector-ref vec index))""")

    // Because the (vector-ref) is unconditionally executed the condition
    // should change the conditional
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("procedure proxying (vector-ref) past possible exception point") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (+ 1 2 3 'not-a-number)
        (vector-ref vec index))""")

    // (+) can can throw an exception
    // This mean (vector-ref) may not be executed
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.DatumCell), vt.IntrinsicCellType(ct.DatumCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("aborted retyping preserves original argument types") {
    val signature = signatureFor("""
      (lambda: ((vec : <vector>) (index : <number>)) 
        (+ 1 2 3 'not-a-number)
        (vector-ref vec index))""")

    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.NumericCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  
  test("procedure proxying (vector-ref) inside a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if (vector? vec)
          (vector-ref vec index)))""")

    // The (vector-ref) is conditionally executed, we can't assert anything about our parameters
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.DatumCell), vt.IntrinsicCellType(ct.DatumCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
}
