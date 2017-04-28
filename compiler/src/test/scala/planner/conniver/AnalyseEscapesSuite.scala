package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.MutableType
import llambda.compiler.{ProcedureSignature, ProcedureAttribute}

import org.scalatest.FunSuite

class AnalyseEscapesSuite extends FunSuite {
  val mutableType = MutableType(vt.Int64)
  val mutableField = mutableType.recordField

  private def escapeAnalyseSteps(steps: List[ps.Step]): List[ps.Step] =
    AnalyseEscapes.conniveSteps(steps)

  test("no steps") {
    assert(escapeAnalyseSteps(Nil) === Nil)
  }

  test("standalone boxing is converted") {
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("boxing captured by return is not converted") {
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.Return(Some(boxedTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("TestCellType does not capture") {
    val resultTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.TestCellType(resultTemp, boxedTemp, ct.IntegerCell)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.TestCellType(resultTemp, boxedTemp, ct.IntegerCell)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("unboxing does not capture") {
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.UnboxInteger(unboxedTemp, boxedTemp)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.UnboxInteger(unboxedTemp, boxedTemp)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CastCellToTypeUnchecked does not capture if the result is not captured") {
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val anyTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.CastCellToTypeUnchecked(anyTemp, boxedTemp, ct.AnyCell)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.CastCellToTypeUnchecked(anyTemp, boxedTemp, ct.AnyCell)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CastCellToTypeUnchecked captures if the result is captured") {
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val anyTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.CastCellToTypeUnchecked(anyTemp, boxedTemp, ct.AnyCell),
      ps.Return(Some(anyTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CondBranch does not capture if the phied result is not captured") {
    val unboxedTemp = ps.TempValue()
    val trueTemp = ps.TempValue()
    val falseTemp = ps.TempValue()
    val phiTemp = ps.TempValue()
    val testTemp = ps.TempValue()

    val valuePhis = List(ps.ValuePhi(phiTemp, trueTemp, falseTemp))

    val inputTrueSteps = List(
      ps.BoxInteger(trueTemp, unboxedTemp, false)
    )
    val inputFalseSteps = List(
      ps.BoxInteger(falseTemp, unboxedTemp, false)
    )
    val inputSteps = List(
      ps.CondBranch(testTemp, inputTrueSteps, inputFalseSteps, valuePhis)
    )

    val expectedTrueSteps = List(
      ps.BoxInteger(trueTemp, unboxedTemp, true)
    )
    val expectedFalseSteps = List(
      ps.BoxInteger(falseTemp, unboxedTemp, true)
    )
    val expectedSteps = List(
      ps.CondBranch(testTemp, expectedTrueSteps, expectedFalseSteps, valuePhis)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CondBranch captures if the phied result is captured") {
    val unboxedTemp = ps.TempValue()
    val trueTemp = ps.TempValue()
    val falseTemp = ps.TempValue()
    val phiTemp = ps.TempValue()
    val testTemp = ps.TempValue()

    val valuePhis = List(ps.ValuePhi(phiTemp, trueTemp, falseTemp))

    val inputTrueSteps = List(
      ps.BoxInteger(trueTemp, unboxedTemp, false)
    )
    val inputFalseSteps = List(
      ps.BoxInteger(falseTemp, unboxedTemp, false)
    )
    val inputSteps = List(
      ps.CondBranch(testTemp, inputTrueSteps, inputFalseSteps, valuePhis),
      ps.Return(Some(phiTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CondBranch does not capture if the branches do not capture") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val trueTemp = ps.TempValue()
    val falseTemp = ps.TempValue()
    val testTemp = ps.TempValue()

    val trueSteps = List(
      ps.UnboxInteger(trueTemp, boxedTemp)
    )
    val falseSteps = List(
      ps.UnboxInteger(falseTemp, boxedTemp)
    )
    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.CondBranch(testTemp, trueSteps, falseSteps, Nil)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.CondBranch(testTemp, trueSteps, falseSteps, Nil)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CondBranch does capture if the branches do capture") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val trueTemp = ps.TempValue()
    val testTemp = ps.TempValue()

    val trueSteps = List(
      ps.UnboxInteger(trueTemp, boxedTemp)
    )
    val falseSteps = List(
      ps.Return(Some(boxedTemp))
    )
    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.CondBranch(testTemp, trueSteps, falseSteps, Nil)
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("InitPair does not capture if the pair itself is not captured") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val pairTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitPair(pairTemp, boxedTemp, boxedTemp, None, false)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.InitPair(pairTemp, boxedTemp, boxedTemp, None, true)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("InitPair does capture if the pair itself is captured") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val pairTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitPair(pairTemp, boxedTemp, boxedTemp, None, false),
      ps.Return(Some(pairTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("Invoke does not capture if the signature has the NoCapture attribute") {
    val noCaptureSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set(ProcedureAttribute.NoCapture)
    )
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val entryPointTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.Invoke(None, noCaptureSignature, entryPointTemp, List(boxedTemp))
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true),
      ps.Invoke(None, noCaptureSignature, entryPointTemp, List(boxedTemp))
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("Invoke does capture if the signature does not have the NoCapture attribute") {
    val captureSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val entryPointTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.Invoke(None, captureSignature, entryPointTemp, List(boxedTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("TailCall does capture even if the signature has a NoCapture attribute") {
    val noCaptureSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set(ProcedureAttribute.NoCapture)
    )
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val entryPointTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.TailCall(noCaptureSignature, entryPointTemp, List(boxedTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("mutable InitRecord can be stack allocated if the record is not captured") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val recordTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitRecord(recordTemp, mutableType, Map(mutableField -> boxedTemp), isUndefined=false, stackAllocate=false)
    )

    val expectedSteps = List(
      // We assume all of our inputs are captured because we do not track field captures
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitRecord(recordTemp, mutableType, Map(mutableField -> boxedTemp), isUndefined=false, stackAllocate=true)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("mutable InitRecord cannot be stack allocated if the record is captured") {
    val unboxedTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val recordTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitRecord(recordTemp, mutableType, Map(mutableField -> boxedTemp), isUndefined=false, stackAllocate=false),
      ps.Return(Some(recordTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("mutable InitRecord can be stack allocated if one of its fields is loaded and captured") {
    val recordTemp = ps.TempValue()
    val loadedTemp = ps.TempValue()

    val inputSteps = List(
      ps.InitRecord(recordTemp, mutableType, Map(), isUndefined=false, stackAllocate=false),
      ps.LoadRecordLikeFields(recordTemp, mutableType, List(mutableField -> loadedTemp)),
      ps.Return(Some(loadedTemp))
    )

    val expectedSteps = List(
      ps.InitRecord(recordTemp, mutableType, Map(), isUndefined=false, stackAllocate=true),
      ps.LoadRecordLikeFields(recordTemp, mutableType, List(mutableField -> loadedTemp)),
      ps.Return(Some(loadedTemp))
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("setting a mutable field captures the stored value") {
    val recordTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()
    val storedTemp = ps.TempValue()

    val inputSteps = List(
      ps.BoxInteger(storedTemp, unboxedTemp, stackAllocate=false),
      ps.InitRecord(recordTemp, mutableType, Map(), isUndefined=false, stackAllocate=false),
      ps.SetRecordLikeFields(recordTemp, mutableType, List(storedTemp -> mutableField))
    )

    val expectedSteps = List(
      ps.BoxInteger(storedTemp, unboxedTemp, stackAllocate=false),
      ps.InitRecord(recordTemp, mutableType, Map(), isUndefined=false, stackAllocate=true),
      ps.SetRecordLikeFields(recordTemp, mutableType, List(storedTemp -> mutableField))
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }
}
