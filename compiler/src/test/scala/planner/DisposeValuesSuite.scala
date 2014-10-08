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
    fixedArgTypes=List(vt.ExactIntegerType),
    restArgMemberTypeOpt=Some(vt.SymbolType),
    returnType=vt.ReturnType.SingleValue(vt.PortType),
    attributes=Set()
  )

  def namedTemp(valueType : vt.ValueType, name : String) : ps.TempValue = 
    new ps.TempValue(valueType.isGcManaged) {
      override def toString = name
    }

  val worldPtrTemp = new ps.WorldPtrValue 

  val selfTemp = namedTemp(vt.TopProcedureType, "self")
  val fixedArgTemp = namedTemp(vt.ExactIntegerType, "fixedArg")
  val restArgTemp = namedTemp(vt.ListElementType, "restArg")

  def functionForSteps(steps : List[ps.Step]) : PlannedFunction =
    PlannedFunction(
      signature=testSignature,
      namedArguments=List(
        "world" -> worldPtrTemp,
        "self" -> selfTemp,
        "fixedArg" -> fixedArgTemp,
        "restArg" -> restArgTemp
      ),
      steps=steps,
      worldPtrOpt=Some(worldPtrTemp),
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
      ps.UnboxExactInteger(unboxResult, fixedArgTemp)
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
      ps.UnboxExactInteger(unboxResult, fixedArgTemp),
      ps.Return(Some(unboxResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp, restArgTemp)),
      ps.UnboxExactInteger(unboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp)),
      ps.Return(Some(unboxResult)),
      ps.DisposeValues(Set(unboxResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("condition with unused test value discards at top of branches") {
    val trueUnboxResult = namedTemp(vt.Int64, "trueUnboxResult")

    val trueSteps = List(
      ps.UnboxExactInteger(trueUnboxResult, fixedArgTemp)
    )
    
    val falseUnboxResult = namedTemp(vt.Int64, "falseUnboxResult")

    val falseSteps = List(
      ps.UnboxExactInteger(falseUnboxResult, fixedArgTemp)
    )

    val condResult = namedTemp(vt.Int64, "condResult")

    val testSteps = List(
      ps.CondBranch(condResult, restArgTemp, trueSteps, trueUnboxResult, falseSteps, falseUnboxResult),
      ps.Return(Some(condResult))
    )

    val testFunction = functionForSteps(testSteps)

    val expectedTrueSteps = List(
      ps.DisposeValues(Set(restArgTemp)),
      ps.UnboxExactInteger(trueUnboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp))
    )
    
    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp)),
      ps.UnboxExactInteger(falseUnboxResult, fixedArgTemp),
      ps.DisposeValues(Set(fixedArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(condResult, restArgTemp, expectedTrueSteps, trueUnboxResult, expectedFalseSteps, falseUnboxResult),
      ps.Return(Some(condResult)),
      ps.DisposeValues(Set(condResult))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("condition with unused result value discards result immediately after") {
    val condResult = namedTemp(vt.Int64, "condResult")

    val testSteps = List(
      ps.CondBranch(condResult, restArgTemp, Nil, fixedArgTemp, Nil, fixedArgTemp)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedBranchSteps = List(
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(condResult, restArgTemp, expectedBranchSteps, fixedArgTemp, expectedBranchSteps, fixedArgTemp),
      ps.DisposeValues(Set(condResult, fixedArgTemp))
    )

    assert(DisposeValues(testFunction).steps === expectedSteps)
  }
  
  test("condition using outer value as branch result can dispose that value") {
    val condResult = namedTemp(vt.ExactIntegerType, "condResult")

    val trueResult = namedTemp(vt.ExactIntegerType, "trueResult")
    val trueSteps = List(
      ps.CreateExactIntegerCell(trueResult, 25)
    )

    val falseSteps = Nil

    val testSteps = List(
      ps.CondBranch(condResult, restArgTemp, trueSteps, trueResult, Nil, fixedArgTemp)
    )

    val testFunction = functionForSteps(testSteps)

    // This can dispose the fixed arg temp immedately
    val expectedTrueSteps = List(
      ps.DisposeValues(Set(restArgTemp, fixedArgTemp)),
      ps.CreateExactIntegerCell(trueResult, 25)
    )

    val expectedFalseSteps = List(
      ps.DisposeValues(Set(restArgTemp))
    )

    val expectedSteps = List(
      ps.DisposeValues(Set(selfTemp)),
      ps.CondBranch(condResult, restArgTemp, expectedTrueSteps, trueResult, expectedFalseSteps, fixedArgTemp),
      ps.DisposeValues(Set(condResult, fixedArgTemp))
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
