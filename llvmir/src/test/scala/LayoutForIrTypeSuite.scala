package io.llambda.llvmir

import org.scalatest.FunSuite


class LayoutForIrTypeSuite extends FunSuite {
  val defaultDataLayout = DataLayout()
  val x8664MacOSXDataLyout = DataLayout.fromDataLayoutString("e-m:e-i64:64-f80:128-n8:16:32:64-S128")
  val i686MacOSXDataLyout = DataLayout.fromDataLayoutString("e-m:o-p:32:32-f64:32:64-f80:128-n8:16:32-S128")
  val aarch64LinuxDataLayout = DataLayout.fromDataLayoutString("e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128")
  val systemzLinuxDataLayout = DataLayout.fromDataLayoutString("E-m:e-i1:8:16-i8:8:16-i64:64-f128:64-a:8:16-n32:64")

  test("pointer layouts") {
    val pointerType = PointerType(IntegerType(32))

    assert(LayoutForIrType(defaultDataLayout)(pointerType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(pointerType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(pointerType) === TypeLayout(32, 32, 32))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(pointerType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(pointerType) === TypeLayout(64, 64, 64))
  }

  test("exact integer layouts") {
    val integerType = IntegerType(16)

    assert(LayoutForIrType(defaultDataLayout)(integerType) === TypeLayout(16, 16, 16))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(integerType) === TypeLayout(16, 16, 16))
    assert(LayoutForIrType(i686MacOSXDataLyout)(integerType) === TypeLayout(16, 16, 16))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(integerType) === TypeLayout(16, 16, 32))
    assert(LayoutForIrType(systemzLinuxDataLayout)(integerType) === TypeLayout(16, 16, 16))
  }

  test("fuzzy integer layouts") {
    val integerType = IntegerType(17)

    assert(LayoutForIrType(defaultDataLayout)(integerType) === TypeLayout(17, 32, 32))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(integerType) === TypeLayout(17, 32, 32))
    assert(LayoutForIrType(i686MacOSXDataLyout)(integerType) === TypeLayout(17, 32, 32))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(integerType) === TypeLayout(17, 32, 32))
    assert(LayoutForIrType(systemzLinuxDataLayout)(integerType) === TypeLayout(17, 32, 32))
  }

  test("oversized integer layouts") {
    val integerType = IntegerType(256)

    assert(LayoutForIrType(defaultDataLayout)(integerType) === TypeLayout(256, 32, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(integerType) === TypeLayout(256, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(integerType) === TypeLayout(256, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(integerType) === TypeLayout(256, 128, 128))
    assert(LayoutForIrType(systemzLinuxDataLayout)(integerType) === TypeLayout(256, 64, 64))
  }

  test("float layouts") {
    val floatType = FloatType

    assert(LayoutForIrType(defaultDataLayout)(floatType) === TypeLayout(32, 32, 32))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(floatType) === TypeLayout(32, 32, 32))
    assert(LayoutForIrType(i686MacOSXDataLyout)(floatType) === TypeLayout(32, 32, 32))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(floatType) === TypeLayout(32, 32, 32))
    assert(LayoutForIrType(systemzLinuxDataLayout)(floatType) === TypeLayout(32, 32, 32))
  }

  test("double layouts") {
    val doubleType = DoubleType

    assert(LayoutForIrType(defaultDataLayout)(doubleType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(doubleType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(doubleType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(doubleType) === TypeLayout(64, 64, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(doubleType) === TypeLayout(64, 64, 64))
  }

  test("i1 array") {
    val arrayType = ArrayType(10, IntegerType(1))

    assert(LayoutForIrType(defaultDataLayout)(arrayType) === TypeLayout(80, 8, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(arrayType) === TypeLayout(80, 8, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(arrayType) === TypeLayout(80, 8, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(arrayType) === TypeLayout(80, 8, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(arrayType) === TypeLayout(80, 8, 16))
  }

  test("empty i8 array") {
    val arrayType = ArrayType(0, IntegerType(1))

    assert(LayoutForIrType(defaultDataLayout)(arrayType) === TypeLayout(0, 8, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(arrayType) === TypeLayout(0, 8, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(arrayType) === TypeLayout(0, 8, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(arrayType) === TypeLayout(0, 8, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(arrayType) === TypeLayout(0, 8, 16))
  }

  test("metadata layouts") {
    val metadataType = MetadataType

    assert(LayoutForIrType(defaultDataLayout)(metadataType) === TypeLayout(0, 0, 0))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(metadataType) === TypeLayout(0, 0, 0))
    assert(LayoutForIrType(i686MacOSXDataLyout)(metadataType) === TypeLayout(0, 0, 0))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(metadataType) === TypeLayout(0, 0, 0))
    assert(LayoutForIrType(systemzLinuxDataLayout)(metadataType) === TypeLayout(0, 0, 0))
  }

  test("empty struct") {
    val structType = StructureType(Nil)

    assert(LayoutForIrType(defaultDataLayout)(structType) === TypeLayout(0, 0, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(structType) === TypeLayout(0, 0, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(structType) === TypeLayout(0, 0, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(structType) === TypeLayout(0, 0, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(structType) === TypeLayout(0, 8, 16))
  }

  test("trivial struct 1") {
    val structType = StructureType(List(
      IntegerType(8),
      IntegerType(32)
    ))

    assert(LayoutForIrType(defaultDataLayout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(structType) === TypeLayout(64, 32, 32))
  }

  test("trivial struct 2") {
    val structType = StructureType(List(
      IntegerType(32),
      IntegerType(8)
    ))

    assert(LayoutForIrType(defaultDataLayout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(structType) === TypeLayout(64, 32, 32))
  }

  test("struct with empty array") {
    val structType = StructureType(List(
      IntegerType(32),
      ArrayType(0, IntegerType(64)),
      IntegerType(8)
    ))

    assert(LayoutForIrType(defaultDataLayout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(structType) === TypeLayout(128, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(structType) === TypeLayout(64, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(structType) === TypeLayout(128, 64, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(structType) === TypeLayout(128, 64, 64))
  }

  test("PairCell struct") {
    val anyCellType = StructureType(List(
      IntegerType(8),
      IntegerType(8)
    ))

    val pairCellType = StructureType(List(
      anyCellType,
      IntegerType(32),
      PointerType(anyCellType),
      PointerType(anyCellType)
    ))

    assert(LayoutForIrType(defaultDataLayout)(pairCellType) === TypeLayout(192, 64, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(pairCellType) === TypeLayout(192, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(pairCellType) === TypeLayout(128, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(pairCellType) === TypeLayout(192, 64, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(pairCellType) === TypeLayout(192, 64, 64))
  }

  test("FlonumCell struct") {
    val anyCellType = StructureType(List(
      IntegerType(8),
      IntegerType(8)
    ))

    val flonumCellType = StructureType(List(
      anyCellType,
      DoubleType
    ))

    assert(LayoutForIrType(defaultDataLayout)(flonumCellType) === TypeLayout(128, 64, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(flonumCellType) === TypeLayout(128, 64, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(flonumCellType) === TypeLayout(96, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(flonumCellType) === TypeLayout(128, 64, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(flonumCellType) === TypeLayout(128, 64, 64))
  }

  test("nested structs") {
    val innerStructType = StructureType(List(
      IntegerType(32),
      IntegerType(8)
    ))

    val outerStructType = StructureType(List(
      innerStructType,
      IntegerType(8)
    ))

    assert(LayoutForIrType(defaultDataLayout)(outerStructType) === TypeLayout(96, 32, 64))
    assert(LayoutForIrType(x8664MacOSXDataLyout)(outerStructType) === TypeLayout(96, 32, 64))
    assert(LayoutForIrType(i686MacOSXDataLyout)(outerStructType) === TypeLayout(96, 32, 64))
    assert(LayoutForIrType(aarch64LinuxDataLayout)(outerStructType) === TypeLayout(96, 32, 64))
    assert(LayoutForIrType(systemzLinuxDataLayout)(outerStructType) === TypeLayout(96, 32, 32))
  }

  test("layout of a user defined type fails") {
    intercept[UnableToDetermineLayoutException] {
      LayoutForIrType(defaultDataLayout)(UserDefinedType("foo"))
    }
  }
}
