package io.llambda.llvmir

import org.scalatest.FunSuite


class DataLayoutSuite extends FunSuite {
  test("default data layout") {
    val dataLayout = DataLayout()

    assert(dataLayout.endian === Endian.Big)
    assert(dataLayout.stackAlignmentBits === 0)
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 32, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.ELF)
    assert(dataLayout.nativeIntegerBits === Set(8, 16, 32, 64))
  }

  test("x86_64 linux data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:e-i64:64-f80:128-n8:16:32:64-S128")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 128)
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === Some(DataAlignment(80, 128, 128)))

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.ELF)
    assert(dataLayout.nativeIntegerBits === Set(8, 16, 32, 64))
  }

  test("i686 macosx data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:o-p:32:32-f64:32:64-f80:128-n8:16:32-S128")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 128)
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 32, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 32, 64))
    assert(dataLayout.floatAlignments.get(80) === Some(DataAlignment(80, 128, 128)))

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.MachO)
    assert(dataLayout.nativeIntegerBits === Set(8, 16, 32))
  }

  test("x86_64 win32 data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:w-i64:64-f80:128-n8:16:32:64-S12")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 12) // ???
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === Some(DataAlignment(80, 128, 128)))

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.WinCOFF)
    assert(dataLayout.nativeIntegerBits === Set(8, 16, 32, 64))
  }

  test("i686 win32 data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:x-p:32:32-i64:64-f80:32-n8:16:32-a:0:32-S3")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 3) // ???
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 32))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === Some(DataAlignment(80, 32, 32)))

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.Win86COFF)
    assert(dataLayout.nativeIntegerBits === Set(8, 16, 32))
  }

  test("arm macosx data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:o-p:32:32-f64:32:64-v64:32:64-v128:32:128-a:0:32-n32-S32")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 32)
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 32))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 32, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 32, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 32, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 32, 128))

    assert(dataLayout.mangling === Mangling.MachO)
    assert(dataLayout.nativeIntegerBits === Set(32))
  }

  test("arm win32 data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:w-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 64)
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 32))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 64, 128))

    assert(dataLayout.mangling === Mangling.WinCOFF)
    assert(dataLayout.nativeIntegerBits === Set(32))
  }

  test("aarch64 linux data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128")

    assert(dataLayout.endian === Endian.Little)
    assert(dataLayout.stackAlignmentBits === 128)
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 32))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.ELF)
    assert(dataLayout.nativeIntegerBits === Set(32, 64))
  }

  test("ppc macosx data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("E-m:o-p:32:32-f64:32:64-n32")

    assert(dataLayout.endian === Endian.Big)
    assert(dataLayout.stackAlignmentBits === 0)
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 32, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 32, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.MachO)
    assert(dataLayout.nativeIntegerBits === Set(32))
  }

  test("ppc64 macosx data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("E-m:o-i64:64-n32:64")

    assert(dataLayout.endian === Endian.Big)
    assert(dataLayout.stackAlignmentBits === 0)
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 8))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.MachO)
    assert(dataLayout.nativeIntegerBits === Set(32, 64))
  }

  test("mips linux data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64")

    assert(dataLayout.endian === Endian.Big)
    assert(dataLayout.stackAlignmentBits === 64)
    assert(dataLayout.pointerAlignment === DataAlignment(32, 32, 32))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 0, 64))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 8))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 32))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.MIPS)
    assert(dataLayout.nativeIntegerBits === Set(32))
  }

  test("systemz linux data layout") {
    val dataLayout = DataLayout.fromDataLayoutString("E-m:e-i1:8:16-i8:8:16-i64:64-f128:64-a:8:16-n32:64")

    assert(dataLayout.endian === Endian.Big)
    assert(dataLayout.stackAlignmentBits === 0)
    assert(dataLayout.pointerAlignment === DataAlignment(64, 64, 64))
    assert(dataLayout.aggregateAlignment === DataAlignment(0, 8, 16))

    assert(dataLayout.integerAlignments(1) === DataAlignment(1, 8, 16))
    assert(dataLayout.integerAlignments(8) === DataAlignment(8, 8, 16))
    assert(dataLayout.integerAlignments(32) === DataAlignment(32, 32, 32))
    assert(dataLayout.integerAlignments(64) === DataAlignment(64, 64, 64))

    assert(dataLayout.floatAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.floatAlignments.get(80) === None)

    assert(dataLayout.vectorAlignments(64) === DataAlignment(64, 64, 64))
    assert(dataLayout.vectorAlignments(128) === DataAlignment(128, 128, 128))

    assert(dataLayout.mangling === Mangling.ELF)
    assert(dataLayout.nativeIntegerBits === Set(32, 64))
  }
}
