package io.llambda.llvmir

import org.scalatest.FunSuite

class AggregateInstrsSuite extends IrTestSuite {
  test("0 index extractvalue fails") {
    val fakeAgg = LocalVariable("fake", UserDefinedType("opaqueType"))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.extractvalue("zeroindex")(
        valueType=IntegerType(8),
        aggregateValue=fakeAgg,
        indices=List()
      )
    }
  }

  test("extractvalue on non-aggregate value fails") {
    val fakeAgg = LocalVariable("fake", IntegerType(64))

    val block = createTestBlock()

    intercept[InconsistentIrException] {
      block.extractvalue("nonagg")(
        valueType=IntegerType(8),
        aggregateValue=fakeAgg,
        indices=List(42)
      )
    }
  }

  test("1 index extractvalue") {
    val fakeAgg = LocalVariable("fake", UserDefinedType("opaqueType"))

    val block = createTestBlock()
    val resultVar = block.extractvalue("oneindex")(
      valueType=IntegerType(8),
      aggregateValue=fakeAgg,
      indices=List(42)
    )

    assert(resultVar.irType === IntegerType(8))
    assertInstr(block, "%oneindex1 = extractvalue %opaqueType %fake, 42")
  }

  test("2 index extractvalue") {
    val fakeAgg = LocalVariable("fake", UserDefinedType("opaqueType"))

    val block = createTestBlock()
    val resultVar = block.extractvalue("twoindex")(
      valueType=IntegerType(8),
      aggregateValue=fakeAgg,
      indices=List(42, 31)
    )

    assert(resultVar.irType === IntegerType(8))
    assertInstr(block, "%twoindex1 = extractvalue %opaqueType %fake, 42, 31")
  }
}
