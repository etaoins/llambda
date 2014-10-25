package io.llambda.compiler.conniver
import io.llambda

import io.llambda.compiler.{valuetype => vt}
import io.llambda.compiler.{celltype => ct}
import io.llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class UnboxEarlySuite extends FunSuite {
  private def unboxEarly(steps : List[ps.Step]) : List[ps.Step] = {
    UnboxEarly.conniveSteps(steps)
  }

  test("no steps") {
    assert(unboxEarly(Nil) === Nil)
  }

  test("optimal unboxing is preserved") {
    val initialTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp, initialTemp),
      ps.UnboxExactInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps = inputSteps

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing is moved across unrelated steps") {
    val initialTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

    val symbolTemp = ps.Temp(vt.SymbolType)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp, initialTemp),
      // There is no need for the boxed exact integer to be live here if the unboxing is moved earlier
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxExactInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp, initialTemp),
      ps.UnboxExactInteger(unboxedTemp, boxedTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.Return(Some(unboxedTemp))
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("moving multiple unboxing steps") {
    val initialTemp = ps.Temp(vt.Int64)

    val boxedTemp1 = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp1 = ps.Temp(vt.Int64)
    val boxedTemp2 = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp2 = ps.Temp(vt.Int64)

    val symbolTemp = ps.Temp(vt.SymbolType)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp1, initialTemp),
      ps.BoxExactInteger(boxedTemp2, initialTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxExactInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxExactInteger(unboxedTemp2, boxedTemp2)
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp1, initialTemp),
      ps.UnboxExactInteger(unboxedTemp1, boxedTemp1),
      ps.BoxExactInteger(boxedTemp2, initialTemp),
      ps.UnboxExactInteger(unboxedTemp2, boxedTemp2),
      ps.CreateSymbolCell(symbolTemp, "Test")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing won't cross step using boxed version as input") {
    val initialTemp = ps.Temp(vt.Int64)
    val boxedTemp = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp = ps.Temp(vt.Int64)

    val symbolTemp = ps.Temp(vt.SymbolType)
    val testTemp = ps.Temp(vt.Predicate)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp, initialTemp),
      ps.TestCellType(testTemp, boxedTemp, ct.ExactIntegerCell),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxExactInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(boxedTemp, initialTemp),
      ps.TestCellType(testTemp, boxedTemp, ct.ExactIntegerCell),
      ps.UnboxExactInteger(unboxedTemp, boxedTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.Return(Some(unboxedTemp))
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("external boxed values are unboxed at top") {
    val boxedTemp1 = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp1 = ps.Temp(vt.Int64)
    val boxedTemp2 = ps.Temp(vt.ExactIntegerType)
    val unboxedTemp2 = ps.Temp(vt.Int64)

    val symbolTemp = ps.Temp(vt.SymbolType)

    val inputSteps = List(
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxExactInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxExactInteger(unboxedTemp2, boxedTemp2)
    )

    val expectedSteps  = List(
      ps.UnboxExactInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxExactInteger(unboxedTemp2, boxedTemp2),
      ps.CreateSymbolCell(symbolTemp, "Test")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing in branches") {
    val initialTemp = ps.Temp(vt.Int64)

    val trueBoxedTemp = ps.Temp(vt.ExactIntegerType)
    val trueSymbolTemp = ps.Temp(vt.SymbolType)
    val trueUnboxedTemp = ps.Temp(vt.Int64)

    val inputTrueSteps = List(
      ps.BoxExactInteger(trueBoxedTemp, initialTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "True"),
      ps.UnboxExactInteger(trueUnboxedTemp, trueBoxedTemp)
    )

    val falseBoxedTemp = ps.Temp(vt.ExactIntegerType)
    val falseSymbolTemp = ps.Temp(vt.SymbolType)
    val falseUnboxedTemp = ps.Temp(vt.Int64)

    val inputFalseSteps = List(
      ps.BoxExactInteger(trueBoxedTemp, initialTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "False"),
      ps.UnboxExactInteger(trueUnboxedTemp, trueBoxedTemp)
    )

    val condBranchResult = ps.Temp(vt.Int64)
    val outerBoxedTemp = ps.Temp(vt.ExactIntegerType)
    val outerSymbolTemp = ps.Temp(vt.SymbolType)
    val outerUnboxedTemp = ps.Temp(vt.Int64)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(outerBoxedTemp, initialTemp),
      ps.CondBranch(condBranchResult, outerBoxedTemp, inputTrueSteps, trueUnboxedTemp, inputFalseSteps, falseUnboxedTemp),
      ps.CreateSymbolCell(outerSymbolTemp, "Outer"),
      ps.UnboxExactInteger(outerUnboxedTemp, outerBoxedTemp)
    )

    val expectedTrueSteps = List(
      ps.BoxExactInteger(trueBoxedTemp, initialTemp),
      ps.UnboxExactInteger(trueUnboxedTemp, trueBoxedTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "True")
    )

    val expectedFalseSteps = List(
      ps.BoxExactInteger(trueBoxedTemp, initialTemp),
      ps.UnboxExactInteger(trueUnboxedTemp, trueBoxedTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "False")
    )

    val expectedSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxExactInteger(outerBoxedTemp, initialTemp),
      ps.CondBranch(condBranchResult, outerBoxedTemp, expectedTrueSteps, trueUnboxedTemp, expectedFalseSteps, falseUnboxedTemp),
      ps.UnboxExactInteger(outerUnboxedTemp, outerBoxedTemp),
      ps.CreateSymbolCell(outerSymbolTemp, "Outer")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }
}
