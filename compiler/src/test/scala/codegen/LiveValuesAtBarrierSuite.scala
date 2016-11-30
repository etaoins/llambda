package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import org.scalatest.FunSuite

class LiveValuesAtBarrierSuite extends FunSuite {
  import LiveValuesAtBarrier.Result._

  private val nonWorldSignature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    mandatoryArgTypes=List(vt.IntegerType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.UnitType),
    attributes=Set()
  )

  private val worldSignature = ProcedureSignature(
    hasWorldArg=true,
    hasSelfArg=false,
    mandatoryArgTypes=List(vt.IntegerType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.UnitType),
    attributes=Set()
  )

  private val noReturnSignature = ProcedureSignature(
    hasWorldArg=true,
    hasSelfArg=false,
    mandatoryArgTypes=List(vt.IntegerType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.UnitType),
    attributes=Set(ProcedureAttribute.NoReturn)
  )

  def namedTemp(valueType : vt.ValueType, name : String) : ps.TempValue =
    new ps.TempValue(valueType.isGcManaged) {
      override def toString = name
    }

  test("empty steps have no barrier") {
    val initialValues = Set(namedTemp(vt.IntegerType, "someTemp"))
    val testSteps = Nil

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("invoke without world pointer is not a GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, nonWorldSignature, "test"),
      ps.Invoke(None, nonWorldSignature, entryPointTemp, List(temp1), Set())
    )

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("invoke with world pointer is a GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.Invoke(None, worldSignature, entryPointTemp, List(temp1), Set())
    )

    val expectedResult = BarrierEncountered(Set(temp1, temp2))

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("invoke with world pointer that does not return is not a GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, noReturnSignature, "test"),
      ps.Invoke(None, noReturnSignature, entryPointTemp, List(temp1), Set())
    )

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("tail call with world pointer is not a GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.TailCall(worldSignature, entryPointTemp, List(temp1))
    )

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("dispose removes values from future GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.DisposeValues(Set(temp2)),
      ps.Invoke(None, worldSignature, entryPointTemp, List(temp1), Set())
    )

    val expectedResult = BarrierEncountered(Set(temp1))

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("return prevents a GC barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.Return(None),
      ps.Invoke(None, worldSignature, entryPointTemp, List(temp1), Set())
    )

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("invoke that disposes input values removes them from its barrier") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.Invoke(None, worldSignature, entryPointTemp, List(temp1), Set(temp1))
    )

    val expectedResult = BarrierEncountered(Set(temp2))

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("branches with disjoint rooted values") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val condResult = namedTemp(vt.IntegerType, "condResult")
    val trueResult = namedTemp(vt.IntegerType, "condResult")
    val falseResult = namedTemp(vt.IntegerType, "condResult")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.CondBranch(temp1,
        List(
          ps.Invoke(Some(trueResult), worldSignature, entryPointTemp, List(temp1), Set(temp1))
        ),
        List(
          ps.Invoke(Some(trueResult), worldSignature, entryPointTemp, List(temp2), Set(temp2))
        ),
        List(ps.ValuePhi(condResult, trueResult, falseResult))
      )
    )

    val expectedResult = BarrierEncountered(Set())

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("branches with intersecting rooted values") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val condResult = namedTemp(vt.IntegerType, "condResult")
    val trueResult = namedTemp(vt.IntegerType, "condResult")
    val falseResult = namedTemp(vt.IntegerType, "condResult")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.CondBranch(temp1,
        List(
          ps.Invoke(Some(trueResult), worldSignature, entryPointTemp, List(temp1), Set())
        ),
        List(
          ps.Invoke(Some(falseResult), worldSignature, entryPointTemp, List(temp2), Set())
        ),
        List(ps.ValuePhi(condResult, trueResult, falseResult))
      )
    )

    val expectedResult = BarrierEncountered(Set(temp1, temp2))

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("branches with only one barrier branch") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val condResult = namedTemp(vt.IntegerType, "condResult")
    val trueResult = namedTemp(vt.IntegerType, "condResult")
    val falseResult = namedTemp(vt.IntegerType, "condResult")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.CondBranch(temp1,
        List(
          ps.Invoke(Some(trueResult), worldSignature, entryPointTemp, List(temp1), Set())
        ),
        List(
          ps.TailCall(worldSignature, entryPointTemp, List(temp2))
        ),
        List(ps.ValuePhi(condResult, trueResult, falseResult))
      )
    )

    val expectedResult = NoBarrier

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }

  test("nested barrier branches") {
    val entryPointTemp = ps.EntryPointTemp()
    val temp1 = namedTemp(vt.IntegerType, "temp1")
    val temp2 = namedTemp(vt.IntegerType, "temp2")

    val condResult = namedTemp(vt.IntegerType, "condResult")
    val falseResult = namedTemp(vt.IntegerType, "condResult")

    val nestedCondResult = namedTemp(vt.IntegerType, "condResult")
    val nestedTrueResult = namedTemp(vt.IntegerType, "condResult")
    val nestedFalseResult = namedTemp(vt.IntegerType, "condResult")

    val initialValues = Set(temp1, temp2)

    val testSteps = List(
      ps.CreateNamedEntryPoint(entryPointTemp, worldSignature, "test"),
      ps.CondBranch(temp1,
        List(
          ps.DisposeValues(Set(temp1)),
          ps.CondBranch(temp1,
            List(
              ps.Invoke(Some(nestedFalseResult), worldSignature, entryPointTemp, List(temp1), Set())
            ),
            List(
              ps.Invoke(Some(nestedTrueResult), worldSignature, entryPointTemp, List(temp1), Set())
            ),
            List(ps.ValuePhi(nestedCondResult, nestedTrueResult, nestedFalseResult))
          )
        ),
        List(
          ps.Invoke(Some(falseResult), worldSignature, entryPointTemp, List(temp2), Set())
        ),
        List(ps.ValuePhi(condResult, nestedCondResult, falseResult))
      )
    )

    val expectedResult = BarrierEncountered(Set(temp2))

    assert(LiveValuesAtBarrier(testSteps, initialValues) === expectedResult)
  }
}
