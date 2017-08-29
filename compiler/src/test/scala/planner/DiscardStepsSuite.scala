package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{RuntimeErrorMessage, ErrorCategory}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite


class DiscardStepsSuite extends FunSuite {
  private val temp1 = ps.TempValue()
  private val temp2 = ps.TempValue()

  private def namedTemp(name: String): ps.TempValue = new ps.TempValue {
    override def toString = name
  }

  private val runtimeErrorMessage1 = RuntimeErrorMessage(
    category=ErrorCategory.Arity,
    text="text1"
  )

  private val runtimeErrorMessage2 = RuntimeErrorMessage(
    category=ErrorCategory.Arity,
    text="text2"
  )

  test("unused step is completely removed") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, temp1)
    )

    val expectedSteps = List()

    assert(DiscardSteps(testSteps) === expectedSteps)
  }

  test("simple step with used result") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, temp1),
      ps.Return(Some(unboxResult))
    )

    val expectedSteps = List(
      ps.UnboxInteger(unboxResult, temp1),
      ps.Return(Some(unboxResult))
    )

    assert(DiscardSteps(testSteps) === expectedSteps)
  }

  test("unused condition is completely removed") {
    val condResult = namedTemp("condResult")

    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.CreateIntegerCell(trueResult, 25)
    )

    val falseSteps = Nil

    val valuePhi = ps.ValuePhi(condResult, trueResult, temp1)
    val testSteps = List(
      ps.CondBranch(temp2, trueSteps, falseSteps, List(valuePhi))
    )

    val expectedSteps = List()

    assert(DiscardSteps(testSteps) === expectedSteps)
  }

  test("condition with identical branches is simplified") {
    val condResult = namedTemp("condResult")

    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.SignalError(runtimeErrorMessage2),
      ps.CreateIntegerCell(trueResult, 25),
      ps.SignalError(runtimeErrorMessage1)
    )

    val falseSteps = List(
      ps.SignalError(runtimeErrorMessage2),
      ps.SignalError(runtimeErrorMessage1)
    )

    val valuePhi = ps.ValuePhi(condResult, trueResult, temp1)
    val testSteps = List(
      ps.CondBranch(temp2, trueSteps, falseSteps, List(valuePhi))
    )

    val expectedSteps = List(
      ps.SignalError(runtimeErrorMessage2),
      ps.SignalError(runtimeErrorMessage1)
    )

    assert(DiscardSteps(testSteps) === expectedSteps)
  }

  test("unused load record fields is removed") {
    val recordField1 = new vt.RecordField("field1", vt.Int64, mutable=false)
    val recordField2 = new vt.RecordField("field2", vt.Int64, mutable=false)
    val recordType = new vt.RecordType("<test>", List(recordField1, recordField2))

    val fieldTemp1 = namedTemp("fieldTemp1")
    val fieldTemp2 = namedTemp("fieldTemp2")

    val fieldsToLoad = List(
      (recordField1 -> fieldTemp1),
      (recordField2 -> fieldTemp2)
    )

    val testSteps = List(
      ps.LoadRecordLikeFields(temp1, recordType, fieldsToLoad)
    )

    val expectedSteps = List()

    assert(DiscardSteps(testSteps) === expectedSteps)
  }

  test("partially used load record fields has unused fields removed") {
    val recordField1 = new vt.RecordField("field1", vt.Int64, mutable=false)
    val recordField2 = new vt.RecordField("field2", vt.Int64, mutable=false)
    val recordType = new vt.RecordType("<test>", List(recordField1, recordField2))

    val fieldTemp1 = namedTemp("fieldTemp1")
    val fieldTemp2 = namedTemp("fieldTemp2")

    val testFieldsToLoad = List(
      (recordField1 -> fieldTemp1),
      (recordField2 -> fieldTemp2)
    )

    val testSteps = List(
      ps.LoadRecordLikeFields(temp1, recordType, testFieldsToLoad),
      ps.Return(Some(fieldTemp1))
    )

    val expectedFieldsToLoad = List(
      (recordField1 -> fieldTemp1)
    )

    val expectedSteps = List(
      ps.LoadRecordLikeFields(temp1, recordType, expectedFieldsToLoad),
      ps.Return(Some(fieldTemp1))
    )

    assert(DiscardSteps(testSteps) === expectedSteps)
  }
}
