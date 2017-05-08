package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.platform
import llambda.llvmir._
import llambda.compiler.{valuetype => vt}

class PackRecordLikeSuite extends FunSuite {
  private val targetPlatform = platform.ExamplePlatform.`x86_64-pc-linux-gnu`

  test("empty record can be packed") {
    val unpackedRecord = new vt.RecordType("<test>", Nil)
    val packedRecord = PackRecordLike(None, unpackedRecord, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=Nil,
      sizeBytes=0
    ))
  }

  test("three fields without parent type") {
    val firstField = new vt.RecordField("first", vt.Int32, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int8, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField))

    val packedRecord = PackRecordLike(None, unpackedRecord, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(thirdField, firstField, secondField),
      sizeBytes=16
    ))
  }

  test("three fields with parent type") {
    val structType = StructureType(List(IntegerType(32)))

    val firstField = new vt.RecordField("first", vt.Int32, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int8, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField))

    val packedRecord = PackRecordLike(Some(structType), unpackedRecord, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(thirdField, firstField, secondField),
      sizeBytes=24
    ))
  }

  test("three fields should be stable sort") {
    val structType = StructureType(List(IntegerType(32)))

    val firstField = new vt.RecordField("first", vt.Int32, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int8, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField))

    val packedRecord = PackRecordLike(Some(structType), unpackedRecord, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(thirdField, firstField, secondField),
      sizeBytes=24
    ))
  }
}
