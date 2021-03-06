package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class PlanHeapAllocationsSuite extends FunSuite {
  val testSignature = ProcedureSignature(
    hasWorldArg=true,
    hasSelfArg=false,
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.UnitType),
    attributes=Set()
  )

  def functionForSteps(steps: List[ps.Step]): PlannedFunction =
    PlannedFunction(
      signature=testSignature,
      namedArguments=List(
        "world" -> ps.WorldPtrValue
      ),
      steps=steps,
      debugContextOpt=None
    )

  test("empty steps") {
    val testFunction = functionForSteps(Nil)

    val expectedSteps = Nil

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("single non-consuming step") {
    val nativeResult = ps.TempValue()
    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("single consuming step") {
    val nativeResult = ps.TempValue()
    val boxedResult = ps.TempValue()

    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.AllocateHeapCells(1),
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("multiple consuming steps are merged") {
    val nativeResult = ps.TempValue()
    val boxedResult = ps.TempValue()

    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.AllocateHeapCells(3),
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("cell allocations can not be merged across GC barrier") {
    val nativeResult = ps.TempValue()
    val boxedResult = ps.TempValue()

    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.InitVector(boxedResult, Vector()),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.AllocateHeapCells(2),
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.InitVector(boxedResult, Vector()),
      ps.AllocateHeapCells(1),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("cell allocations are not affected by empty branch") {
    val nativeResult = ps.TempValue()
    val boxedResult = ps.TempValue()
    val condResult = ps.TempValue()
    val valuePhi = ps.ValuePhi(condResult, nativeResult, nativeResult)

    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.CondBranch(boxedResult, Nil, Nil, List(valuePhi)),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedSteps = List(
      ps.AllocateHeapCells(3),
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.CondBranch(boxedResult, Nil, Nil, List(valuePhi)),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }

  test("cell allocations with allocating branch") {
    val nativeResult = ps.TempValue()
    val boxedResult = ps.TempValue()
    val condResult = ps.TempValue()
    val valuePhi = ps.ValuePhi(condResult, nativeResult, nativeResult)

    val trueSteps = List(
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val falseSteps = List(
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testSteps = List(
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.CondBranch(boxedResult, trueSteps, falseSteps, List(valuePhi)),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val testFunction = functionForSteps(testSteps)

    val expectedTrueSteps = List(
      ps.AllocateHeapCells(2),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val expectedFalseSteps = List(
      ps.AllocateHeapCells(1),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    val expectedSteps = List(
      ps.AllocateHeapCells(2),
      ps.CreateNativeInteger(nativeResult, 25, 64),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.BoxInteger(boxedResult, nativeResult),
      ps.CondBranch(boxedResult, expectedTrueSteps, expectedFalseSteps, List(valuePhi)),
      ps.AllocateHeapCells(1),
      ps.BoxInteger(boxedResult, nativeResult)
    )

    assert(PlanHeapAllocations(testFunction).steps === expectedSteps)
  }
}
