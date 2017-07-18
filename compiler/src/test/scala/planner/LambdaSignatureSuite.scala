package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.SchemeStringImplicits._

import llambda.compiler._
import org.scalatest.FunSuite


class LambdaSignatureSuite extends FunSuite with PlanHelpers{
  private def retypingProcedureName = "retyping-procedure"

  private def signatureFor(scheme: String): ProcedureSignature = {
    val importDecl = datum"(import (llambda base) (llambda typed) (llambda test-util))"

    // Give the procedure a distinctive name so we can find it later
    val procedureDatum = ast.ProperList(List(
      ast.Symbol("define"),
      ast.Symbol(retypingProcedureName),
      SchemeParser.parseStringAsData(scheme, None).head
    ))

    // Reference the procedure to force it to be planned
    // Higher optimisation levels would see right through this but this works on -O 0
    val referenceDatum = ast.ProperList(List(
      ast.Symbol("raise"),
      ast.Symbol(retypingProcedureName)
    ))

    val data = List(importDecl, procedureDatum, referenceDatum)

    val functions = planForData(data, optimise=false)
    functions(retypingProcedureName).signature
  }

  test("argless procedure returning integer constant") {
    val signature = signatureFor("""(lambda () 1)""")

    assert(signature.hasWorldArg === false)
    assert(signature.mandatoryArgTypes === Nil)
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.Int64))
  }

  test("procedure returning its argument") {
    val signature = signatureFor("""(lambda (x) x)""")

    // This can be passed anything so it can return anything
    assert(signature.hasWorldArg === false)
    assert(signature.mandatoryArgTypes === List(vt.AnySchemeType))
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("explicitly typed procedure returning its argument") {
    val signature = signatureFor("""(lambda ([x : <integer>]) x)""")

    assert(signature.hasWorldArg === false)
    assert(signature.mandatoryArgTypes === List(vt.Int64))
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.Int64))
  }

  test("explicitly casting mandatory procedure argument to type") {
    val signature = signatureFor("""(lambda (x) (cast x <char>))""")

    assert(signature.mandatoryArgTypes === List(vt.UnicodeChar))
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.UnicodeChar))
  }
/*
  test("explicitly casting optional procedure argument to type") {
    val signature = signatureFor("""(lambda ([x #\a]) (cast x <char>))""")

    assert(signature.mandatoryArgTypes === Nil)
    assert(signature.optionalArgTypes === List(vt.CharType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.UnicodeChar))
  }*/

  test("explicitly casting mandatory procedure argument to type with rest arg") {
    val signature = signatureFor("""(lambda (x .  rest) (cast x <char>))""")

    assert(signature.mandatoryArgTypes === List(vt.UnicodeChar))
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.UnicodeChar))
  }

  test("assigning procedure argument to typed immutable") {
    val signature = signatureFor("""
      (lambda (x)
        (define y : <flonum> x)
        y)"""
    )

    assert(signature.mandatoryArgTypes === List(vt.Double))
    assert(signature.optionalArgTypes === Nil)
    assert(signature.returnType === vt.ReturnType.Reachable(vt.Double))
  }

  test("assigning procedure argument to typed mutable") {
    val signature = signatureFor("""
      (lambda (x)
        (define y : <flonum> x)
        (set! y 5.0)
        y)"""
    )

    assert(signature.mandatoryArgTypes === List(vt.Double))
  }

  test("procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda (vec index) (vector-ref vec index))""")

    // This can be passed anything so it can return anything
    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.Int64))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying (make-vector)") {
    val signature = signatureFor("""(lambda (len fill) (make-vector len fill))""")

    assert(signature.mandatoryArgTypes === List(vt.Int64, vt.AnySchemeType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.VectorType))
  }

  test("typed procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda ([vec : <vector>] [index : <number>]) (vector-ref vec index))""")

    // We should refine <number> in to <integer>
    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.Int64))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("custom union typed procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda ([vec : (U <vector> <char>)] [index : <number>]) (vector-ref vec index))""")

    // We should refine <number> in to <integer>
    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.Int64))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying (vector-set!)") {
    val signature = signatureFor("""(lambda (vec index) (vector-set! vec index #f))""")

    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.Int64))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.UnitType))
  }

  test("types used across (if) branches are unioned together") {
    val signature = signatureFor("""
      (lambda (value)
        (if (dynamic-true)
          (cast value <string>)
          (cast value <symbol>)))""")

    assert(signature.returnType === vt.ReturnType.Reachable(vt.UnionType(Set(vt.StringType, vt.SymbolType))))
  }

  test("procedure proxying (vector-ref) past a conditional") {
    val signature = signatureFor("""
      (lambda (vec index)
        (if #t 1 2)
        (vector-ref vec index))""")

    // Because the (vector-ref) is unconditionally executed the condition
    // should change the conditional
    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.Int64))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying function with typed rest arg") {
    val signature = signatureFor("""
      (lambda (val1 val2)
        (+ 5 val1 val2))""")

    assert(signature.mandatoryArgTypes === List(vt.NumberType, vt.NumberType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.NumberType))
  }

  test("procedure proxying (vector-ref) past possible exception point") {
    val signature = signatureFor("""
      (lambda (vec index)
        (/ 0 0)
        (vector-ref vec index))""")

    // (/) can can throw an exception
    // This mean (vector-ref) may not be executed
    assert(signature.mandatoryArgTypes === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying (vector-ref) past possible exception point in true branch") {
    val signature = signatureFor("""
      (lambda (vec index)
        (if #t (/ 0 0) #f)
        (vector-ref vec index))""")

    assert(signature.mandatoryArgTypes === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying (vector-ref) past possible exception point in false branch") {
    val signature = signatureFor("""
      (lambda (vec index)
        (if #t #f (/ 0 0))
        (vector-ref vec index))""")

    assert(signature.mandatoryArgTypes === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure proxying (>) inside a conditional test") {
    // This makes sure the types from (+) are passed down to the branches
    // (>) is used here because it can't throw exceptions
    val signature = signatureFor("""
      (lambda (m n)
        (if (> 0 m n) #t #f))""")

    assert(signature.mandatoryArgTypes === List(vt.NumberType, vt.NumberType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.Predicate))
  }

  test("aborted retyping preserves original argument types") {
    val signature = signatureFor("""
      (lambda ([vec : <vector>] [index : <number>])
        (/ 0 0)
        (vector-ref vec index))""")

    assert(signature.mandatoryArgTypes === List(vt.VectorType, vt.NumberType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }


  test("procedure proxying (vector-ref) inside a conditional") {
    val signature = signatureFor("""
      (lambda (vec index)
        (if (vector? vec)
          (vector-ref vec index)))""")

    // The (vector-ref) is conditionally executed, we can't assert anything about our parameters
    assert(signature.mandatoryArgTypes === List(vt.AnySchemeType, vt.AnySchemeType))
    assert(signature.returnType === vt.ReturnType.Reachable(vt.AnySchemeType))
  }

  test("procedure unconditionally terminating is annotated as NoReturn") {
    val signature = signatureFor("""
      (lambda ()
        (error "ALWAYS TERMINATE"))
    """)

    assert(signature.attributes.contains(ProcedureAttribute.NoReturn))
  }

  test("procedure unconditionally terminating in branches is annotated as NoReturn") {
    val signature = signatureFor("""
      (lambda ()
        (if dynamic-true
          (error "TERMINATES TRUE")
          (error "TERMINATES FALSE")))
    """)

    assert(signature.attributes.contains(ProcedureAttribute.NoReturn))
  }
}
