package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ProcedureSignature, RuntimeErrorMessage, ErrorCategory}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class DisposeValuesSuite extends FunSuite {
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

  test("empty function disposes its arguments") {
    val testFunction = functionForSteps(Nil)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, fixedArgTemp, restArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("unused step is completely removed") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, fixedArgTemp)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, fixedArgTemp, restArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("simple step with used result") {
    val unboxResult = namedTemp("unboxResult")

    val testSteps = List(
      ps.UnboxInteger(unboxResult, fixedArgTemp),
      ps.Return(Some(unboxResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, restArgTemp)),
      ps.UnboxInteger(unboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp)),
      ps.Return(Some(unboxResult)),
      ps.DisposeValues(Set(unboxResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("condition with unused test value discards at top of branches") {
    val trueUnboxResult = namedTemp("trueUnboxResult")

    val trueSteps = List(
      ps.UnboxInteger(trueUnboxResult, fixedArgTemp)
    )

    val falseUnboxResult = namedTemp("falseUnboxResult")

    val falseSteps = List(
      ps.UnboxInteger(falseUnboxResult, fixedArgTemp)
    )

    val condResult = namedTemp("condResult")

    val valuePhi = ps.ValuePhi(condResult, trueUnboxResult, falseUnboxResult)
    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, falseSteps, List(valuePhi)),
      ps.Return(Some(condResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedTrueSteps = List(
      ps.DisposeValues(Set(restArgTemp)),
      ps.UnboxInteger(trueUnboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp))
    )

    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp)),
      ps.UnboxInteger(falseUnboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(restArgTemp, expectedTrueSteps, expectedFalseSteps, List(valuePhi)),
      ps.Return(Some(condResult)),
      ps.DisposeValues(Set(condResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("condition with unused result value discards phi") {
    val condResult = namedTemp("condResult")

    val valuePhi = ps.ValuePhi(condResult, fixedArgTemp, fixedArgTemp)
    val trueSteps = List(
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage)
    )

    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, Nil, List(valuePhi))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedTrueSteps = List(
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage),
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, fixedArgTemp)),
      ps.CondBranch(restArgTemp, expectedTrueSteps, expectedFalseSteps, Nil)
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("condition using outer value as branch result can dispose that value") {
    val condResult = namedTemp("condResult")

    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.CreateIntegerCell(trueResult, 25)
    )

    val valuePhi = ps.ValuePhi(condResult, trueResult, fixedArgTemp)
    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, Nil, List(valuePhi)),
      ps.Return(Some(condResult))
    )

    val testFunction = functionForSteps(testSteps)

    // This can dispose the fixed arg temp immedately
    val expectedTrueSteps = List(
      ps.DisposeValues(Set(restArgTemp, fixedArgTemp)),
      ps.CreateIntegerCell(trueResult, 25)
    )

    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(restArgTemp, expectedTrueSteps, expectedFalseSteps, List(valuePhi)),
      ps.DisposeValues(Set(fixedArgTemp)),
      ps.Return(Some(condResult)),
      ps.DisposeValues(Set(condResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("condition using outer value in only one branch disposes value in other branch") {
    val condResult = namedTemp("condResult")
    val outerValue = namedTemp("outerValue")

    // Make sure we don't try to unroot internal result in the other branch
    val internalUnboxResult = namedTemp("internalUnbox")
    val internalReboxResult = namedTemp("internalReboxResult")
    val trueResult = namedTemp("trueResult")
    val trueSteps = List(
      ps.UnboxInteger(internalUnboxResult, outerValue),
      ps.BoxInteger(internalReboxResult, internalUnboxResult),
      ps.UnboxInteger(trueResult, internalReboxResult)
    )

    val falseResult = namedTemp("falseResult")
    val falseSteps = List(
      ps.CreateNativeInteger(falseResult, 25, 64)
    )

    val valuePhi = ps.ValuePhi(condResult, trueResult, falseResult)
    val testSteps = List(
      ps.CreateIntegerCell(outerValue, 50),
      ps.CondBranch(restArgTemp, trueSteps, falseSteps, List(valuePhi)),
      ps.Return(Some(condResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedTrueSteps = List(
      ps.DisposeValues(Set(restArgTemp)),
      ps.UnboxInteger(internalUnboxResult, outerValue),
      ps.DisposeValues(Set(outerValue)),
      ps.BoxInteger(internalReboxResult, internalUnboxResult),
      ps.DisposeValues(Set(internalUnboxResult)),
      ps.UnboxInteger(trueResult, internalReboxResult),
      ps.DisposeValues(Set(internalReboxResult))
    )

    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp, outerValue)),
      ps.CreateNativeInteger(falseResult, 25, 64)
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, fixedArgTemp)),
      ps.CreateIntegerCell(outerValue, 50),
      ps.CondBranch(restArgTemp, expectedTrueSteps, expectedFalseSteps, List(valuePhi)),
      ps.Return(Some(condResult)),
      ps.DisposeValues(Set(condResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
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

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, restArgTemp, fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
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
      ps.DisposeValues(Set(selfTemp)),
      ps.AssertPredicate(restArgTemp, runtimeErrorMessage),
      ps.DisposeValues(Set(restArgTemp)),
      ps.AssertPredicate(fixedArgTemp, runtimeErrorMessage),
      ps.DisposeValues(Set(fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("invoke disposing both arguments") {
    val entryPoint = ps.TempValue()
    val invokeResult = namedTemp("invokeResult")

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPoint, testSignature, "native_symbol"),
      ps.Invoke(Some(invokeResult), testSignature, entryPoint, List(
        fixedArgTemp,
        restArgTemp
      )),
      ps.Return(Some(invokeResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CreateNamedEntryPoint(entryPoint, testSignature, "native_symbol"),
      ps.Invoke(Some(invokeResult), testSignature, entryPoint, List(
        fixedArgTemp,
        restArgTemp
      ), Set(fixedArgTemp, restArgTemp, entryPoint)),
      ps.Return(Some(invokeResult)),
      ps.DisposeValues(Set(invokeResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }

  test("invoke disposing one argument and its result") {
    val entryPoint = ps.TempValue()
    val invokeResult = namedTemp("invokeResult")

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPoint, testSignature, "native_symbol"),
      ps.Invoke(Some(invokeResult), testSignature, entryPoint, List(
        fixedArgTemp,
        restArgTemp
      )),
      ps.Return(Some(fixedArgTemp))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CreateNamedEntryPoint(entryPoint, testSignature, "native_symbol"),
      ps.Invoke(Some(invokeResult), testSignature, entryPoint, List(
        fixedArgTemp,
        restArgTemp
      ), Set(restArgTemp, entryPoint)),
      ps.DisposeValues(Set(invokeResult)),
      ps.Return(Some(fixedArgTemp)),
      ps.DisposeValues(Set(fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
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

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, fixedArgTemp, restArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
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
      ps.DisposeValues(Set(selfTemp, restArgTemp)),
      ps.LoadRecordLikeFields(fixedArgTemp, recordType, expectedFieldsToLoad),
      ps.DisposeValues(Set(fixedArgTemp)),
      ps.Return(Some(fieldTemp1)),
      ps.DisposeValues(Set(fieldTemp1))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
}
