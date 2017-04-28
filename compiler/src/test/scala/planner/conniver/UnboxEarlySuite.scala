package io.llambda.compiler.planner.conniver
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class UnboxEarlySuite extends FunSuite {
  private def unboxEarly(steps: List[ps.Step]): List[ps.Step] = {
    UnboxEarly.conniveSteps(steps)
  }

  test("no steps") {
    assert(unboxEarly(Nil) === Nil)
  }

  test("optimal unboxing is preserved") {
    val initialTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp, initialTemp),
      ps.UnboxInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps = inputSteps

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing is moved across unrelated steps") {
    val initialTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val symbolTemp = ps.TempValue()

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp, initialTemp),
      // There is no need for the boxed integer to be live here if the unboxing is moved earlier
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp, initialTemp),
      ps.UnboxInteger(unboxedTemp, boxedTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.Return(Some(unboxedTemp))
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("moving multiple unboxing steps") {
    val initialTemp = ps.TempValue()

    val boxedTemp1 = ps.TempValue()
    val unboxedTemp1 = ps.TempValue()
    val boxedTemp2 = ps.TempValue()
    val unboxedTemp2 = ps.TempValue()

    val symbolTemp = ps.TempValue()

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp1, initialTemp),
      ps.BoxInteger(boxedTemp2, initialTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxInteger(unboxedTemp2, boxedTemp2)
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp1, initialTemp),
      ps.UnboxInteger(unboxedTemp1, boxedTemp1),
      ps.BoxInteger(boxedTemp2, initialTemp),
      ps.UnboxInteger(unboxedTemp2, boxedTemp2),
      ps.CreateSymbolCell(symbolTemp, "Test")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing won't cross step using boxed version as input") {
    val initialTemp = ps.TempValue()
    val boxedTemp = ps.TempValue()
    val unboxedTemp = ps.TempValue()

    val symbolTemp = ps.TempValue()
    val testTemp = ps.TempValue()

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp, initialTemp),
      ps.TestCellType(testTemp, boxedTemp, ct.IntegerCell),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxInteger(unboxedTemp, boxedTemp),
      ps.Return(Some(unboxedTemp))
    )

    val expectedSteps  = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(boxedTemp, initialTemp),
      ps.TestCellType(testTemp, boxedTemp, ct.IntegerCell),
      ps.UnboxInteger(unboxedTemp, boxedTemp),
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.Return(Some(unboxedTemp))
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("external boxed values are unboxed at top") {
    val boxedTemp1 = ps.TempValue()
    val unboxedTemp1 = ps.TempValue()
    val boxedTemp2 = ps.TempValue()
    val unboxedTemp2 = ps.TempValue()

    val symbolTemp = ps.TempValue()

    val inputSteps = List(
      ps.CreateSymbolCell(symbolTemp, "Test"),
      ps.UnboxInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxInteger(unboxedTemp2, boxedTemp2)
    )

    val expectedSteps  = List(
      ps.UnboxInteger(unboxedTemp1, boxedTemp1),
      ps.UnboxInteger(unboxedTemp2, boxedTemp2),
      ps.CreateSymbolCell(symbolTemp, "Test")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }

  test("unboxing in branches") {
    val initialTemp = ps.TempValue()

    val trueBoxedTemp = ps.TempValue()
    val trueSymbolTemp = ps.TempValue()
    val trueUnboxedTemp = ps.TempValue()

    val inputTrueSteps = List(
      ps.BoxInteger(trueBoxedTemp, initialTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "True"),
      ps.UnboxInteger(trueUnboxedTemp, trueBoxedTemp)
    )

    val falseUnboxedTemp = ps.TempValue()

    val inputFalseSteps = List(
      ps.BoxInteger(trueBoxedTemp, initialTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "False"),
      ps.UnboxInteger(trueUnboxedTemp, trueBoxedTemp)
    )

    val condBranchResult = ps.TempValue()
    val outerBoxedTemp = ps.TempValue()
    val outerSymbolTemp = ps.TempValue()
    val outerUnboxedTemp = ps.TempValue()

    val valuePhi = ps.ValuePhi(condBranchResult, trueUnboxedTemp, falseUnboxedTemp)

    val inputSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(outerBoxedTemp, initialTemp),
      ps.CondBranch(outerBoxedTemp, inputTrueSteps, inputFalseSteps, List(valuePhi)),
      ps.CreateSymbolCell(outerSymbolTemp, "Outer"),
      ps.UnboxInteger(outerUnboxedTemp, outerBoxedTemp)
    )

    val expectedTrueSteps = List(
      ps.BoxInteger(trueBoxedTemp, initialTemp),
      ps.UnboxInteger(trueUnboxedTemp, trueBoxedTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "True")
    )

    val expectedFalseSteps = List(
      ps.BoxInteger(trueBoxedTemp, initialTemp),
      ps.UnboxInteger(trueUnboxedTemp, trueBoxedTemp),
      ps.CreateSymbolCell(trueSymbolTemp, "False")
    )

    val expectedSteps = List(
      ps.CreateNativeInteger(initialTemp, 0, 64),
      ps.BoxInteger(outerBoxedTemp, initialTemp),
      ps.CondBranch(outerBoxedTemp, expectedTrueSteps, expectedFalseSteps, List(valuePhi)),
      ps.UnboxInteger(outerUnboxedTemp, outerBoxedTemp),
      ps.CreateSymbolCell(outerSymbolTemp, "Outer")
    )

    assert(unboxEarly(inputSteps) === expectedSteps)
  }
}
