package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlannedFunction
import llambda.compiler.{ProcedureSignature, ProcedureAttribute}

import org.scalatest.FunSuite

class AnalyseEscapesSuite extends FunSuite {
  private def escapeAnalyseSteps(steps: List[ps.Step]): List[ps.Step] =
    AnalyseEscapes.conniveSteps(steps)

  test("no steps") {
    assert(escapeAnalyseSteps(Nil) === Nil)
  }

  test("standalone boxing is converted") {
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false)
    )

    val expectedSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, true)
    )

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("boxing captured by return is not converted") {
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.Return(Some(boxedTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("TestCellType does not capture") {
    val resultTemp = ps.Temp(vt.Predicate)
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)
    val anyTemp = ps.Temp(vt.AnySchemeType)

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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)
    val anyTemp = ps.Temp(vt.AnySchemeType)

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.CastCellToTypeUnchecked(anyTemp, boxedTemp, ct.AnyCell),
      ps.Return(Some(anyTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("CondBranch does not capture if the phied result is not captured") {
    val unboxedTemp = ps.Temp(vt.Int64)
    val trueTemp = ps.Temp(vt.IntegerType)
    val falseTemp = ps.Temp(vt.IntegerType)
    val phiTemp = ps.Temp(vt.IntegerType)
    val testTemp = ps.Temp(vt.Predicate)

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
    val unboxedTemp = ps.Temp(vt.Int64)
    val trueTemp = ps.Temp(vt.IntegerType)
    val falseTemp = ps.Temp(vt.IntegerType)
    val phiTemp = ps.Temp(vt.IntegerType)
    val testTemp = ps.Temp(vt.Predicate)

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
    val unboxedTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.IntegerType)
    val trueTemp = ps.Temp(vt.IntegerType)
    val falseTemp = ps.Temp(vt.IntegerType)
    val testTemp = ps.Temp(vt.Predicate)

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
    val unboxedTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.IntegerType)
    val trueTemp = ps.Temp(vt.IntegerType)
    val falseTemp = ps.Temp(vt.IntegerType)
    val testTemp = ps.Temp(vt.Predicate)

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

  test("InitPair captures even if the pair itself is not captured") {
    val unboxedTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.IntegerType)
    val pairTemp = ps.Temp(vt.AnyPairType)

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitPair(pairTemp, boxedTemp, boxedTemp, None)
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }

  test("InitPair does capture if the pair itself is captured") {
    val unboxedTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.IntegerType)
    val pairTemp = ps.Temp(vt.AnyPairType)

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.InitPair(pairTemp, boxedTemp, boxedTemp, None),
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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)
    val entryPointTemp = ps.EntryPointTemp()

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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)
    val entryPointTemp = ps.EntryPointTemp()

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
    val boxedTemp = ps.Temp(vt.IntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)
    val entryPointTemp = ps.EntryPointTemp()

    val inputSteps = List(
      ps.BoxInteger(boxedTemp, unboxedTemp, false),
      ps.TailCall(noCaptureSignature, entryPointTemp, List(boxedTemp))
    )

    val expectedSteps = inputSteps

    assert(escapeAnalyseSteps(inputSteps) === expectedSteps)
  }
}
