package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
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

  def namedTemp(valueType : vt.ValueType, name : String) : ps.TempValue = 
    new ps.TempValue(valueType.isGcManaged) {
      override def toString = name
    }

  val selfTemp = namedTemp(vt.TopProcedureType, "self")
  val fixedArgTemp = namedTemp(vt.IntegerType, "fixedArg")
  val restArgTemp = namedTemp(vt.ListElementType, "restArg")

  def functionForSteps(steps : List[ps.Step]) : PlannedFunction =
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
    val unboxResult = namedTemp(vt.Int64, "unboxResult")

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
    val unboxResult = namedTemp(vt.Int64, "unboxResult")

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
    val trueUnboxResult = namedTemp(vt.Int64, "trueUnboxResult")

    val trueSteps = List(
      ps.UnboxInteger(trueUnboxResult, fixedArgTemp)
    )
    
    val falseUnboxResult = namedTemp(vt.Int64, "falseUnboxResult")

    val falseSteps = List(
      ps.UnboxInteger(falseUnboxResult, fixedArgTemp)
    )

    val condResult = namedTemp(vt.Int64, "condResult")

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

  test("condition with unused result value discards result immediately after") {
    val condResult = namedTemp(vt.Int64, "condResult")

    val valuePhi = ps.ValuePhi(condResult, fixedArgTemp, fixedArgTemp)
    val testSteps = List(
      ps.CondBranch(restArgTemp, Nil, Nil, List(valuePhi))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedBranchSteps = List(
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(restArgTemp, expectedBranchSteps, expectedBranchSteps, List(valuePhi)),
      ps.DisposeValues(Set(condResult, fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("condition using outer value as branch result can dispose that value") {
    val condResult = namedTemp(vt.IntegerType, "condResult")

    val trueResult = namedTemp(vt.IntegerType, "trueResult")
    val trueSteps = List(
      ps.CreateIntegerCell(trueResult, 25)
    )

    val falseSteps = Nil

    val valuePhi = ps.ValuePhi(condResult, trueResult, fixedArgTemp)
    val testSteps = List(
      ps.CondBranch(restArgTemp, trueSteps, Nil, List(valuePhi))
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
      ps.DisposeValues(Set(condResult, fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("condition using outer value in only one branch disposes value in other branch") {
    val condResult = namedTemp(vt.Int64, "condResult")
    val outerValue = namedTemp(vt.IntegerType, "outerValue")

    // Make sure we don't try to unroot internal result in the other branch
    val internalUnboxResult = namedTemp(vt.Int64, "internalUnbox")
    val internalReboxResult = namedTemp(vt.IntegerType, "internalReboxResult")
    val trueResult = namedTemp(vt.Int64, "trueResult")
    val trueSteps = List(
      ps.UnboxInteger(internalUnboxResult, outerValue),
      ps.BoxInteger(internalReboxResult, internalUnboxResult),
      ps.UnboxInteger(trueResult, internalReboxResult)
    )

    val falseResult = namedTemp(vt.Int64, "falseResult")
    val falseSteps = List(
      ps.CreateNativeInteger(falseResult, 25, 64)
    )

    val valuePhi = ps.ValuePhi(condResult, trueResult, falseResult)
    val testSteps = List(
      ps.CreateIntegerCell(outerValue, 50),
      ps.CondBranch(restArgTemp, trueSteps, falseSteps, List(valuePhi))
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
      ps.DisposeValues(Set(condResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("invoke disposing both arguments") {
    val entryPoint = ps.EntryPointTemp()
    val invokeResult = namedTemp(vt.Int64, "invokeResult")

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
    val entryPoint = ps.EntryPointTemp()
    val invokeResult = namedTemp(vt.Int64, "invokeResult")

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
}
