package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ProcedureSignature, RuntimeErrorMessage, ErrorCategory}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class DiscardStepsSuite extends FunSuite {
  val testSignature = ProcedureSignature(
    hasWorldArg=true,
    hasSelfArg=true,
    mandatoryArgTypes=List(vt.IntegerType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=Some(vt.SymbolType),
    returnType=vt.ReturnType.Reachable(vt.PortType),
    attributes=Set()
  )

  def namedTemp(name: String): ps.TempValue = new ps.TempValue {
    override def toString = name
  }

  val selfTemp = namedTemp("self")
  val fixedArgTemp = namedTemp("fixedArg")
  val restArgTemp = namedTemp("restArg")

  val runtimeErrorMessage = RuntimeErrorMessage(
    category=ErrorCategory.Arity,
    name="name",
    text="text"
  )

  def functionForSteps(steps: List[ps.Step]): PlannedFunction =
    PlannedFunction(
      signature=testSignature,
      namedArguments=List(
        "world" -> ps.WorldPtrValue,
        "self" -> selfTemp,
        "fixedArg" -> fixedArgTemp,
        "restArg" -> restArgTemp
      ),
      steps=steps,
      debugContextOpt=None
    )

  test("unused step is completely removed") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, fixedArgTemp)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List()

    assert(DiscardSteps(testFunction).steps === expectedSteps)
  }

  test("simple step with used result") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, fixedArgTemp),
      ps.Return(Some(unboxResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.UnboxInteger(unboxResult, fixedArgTemp),
      ps.Return(Some(unboxResult))
    )

    assert(DiscardSteps(testFunction).steps === expectedSteps)
  }

  test("unused condition is completely removed") {
    val condResult = namedTemp("condResult")

    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.CreateIntegerCell(trueResult, 25)
    )

    val falseSteps = Nil

    val valuePhi = ps.ValuePhi(condResult, trueResult, fixedArgTemp)
    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, falseSteps, List(valuePhi))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List()

    assert(DiscardSteps(testFunction).steps === expectedSteps)
  }

  test("condition with identical branches is simplified") {
    val condResult = namedTemp("condResult")

    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage),
      ps.CreateIntegerCell(trueResult, 25),
      ps.AssertPredicate(fixedArgTemp, runtimeErrorMessage)
    )

    val falseSteps = List(
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage),
      ps.AssertPredicate(fixedArgTemp, runtimeErrorMessage)
    )

    val valuePhi = ps.ValuePhi(condResult, trueResult, fixedArgTemp)
    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, falseSteps, List(valuePhi))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage),
      ps.AssertPredicate(fixedArgTemp, runtimeErrorMessage)
    )

    assert(DiscardSteps(testFunction).steps === expectedSteps)
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
      ps.LoadRecordLikeFields(fixedArgTemp, recordType, fieldsToLoad)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List()

    assert(DiscardSteps(testFunction).steps === expectedSteps)
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
      ps.LoadRecordLikeFields(fixedArgTemp, recordType, testFieldsToLoad),
      ps.Return(Some(fieldTemp1))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedFieldsToLoad = List(
      (recordField1 -> fieldTemp1)
    )

    val expectedSteps = List(
      ps.LoadRecordLikeFields(fixedArgTemp, recordType, expectedFieldsToLoad),
      ps.Return(Some(fieldTemp1))
    )

    assert(DiscardSteps(testFunction).steps === expectedSteps)
  }
}
