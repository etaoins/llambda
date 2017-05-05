package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.platform
import llambda.compiler.{valuetype => vt}

class PackRecordLikeSuite extends FunSuite {
  private val targetPlatform = platform.ExamplePlatform.`x86_64-pc-linux-gnu`

  test("empty record can be packed") {
    val unpackedRecord = new vt.RecordType("<test>", Nil)
    val packedRecord = PackRecordLike(unpackedRecord, 16, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=Nil,
      inline=true
    ))
  }

  test("one field that fits is packed inline") {
    val onlyField = new vt.RecordField("only", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(onlyField))

    val packedRecord = PackRecordLike(unpackedRecord, 16, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(onlyField),
      inline=true
    ))
  }

  test("one field that does not fit is packed out-of-line") {
    val onlyField = new vt.RecordField("only", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(onlyField))

    val packedRecord = PackRecordLike(unpackedRecord, 4, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(onlyField),
      inline=false
    ))
  }

  test("three fields that already fit") {
    val firstField = new vt.RecordField("first", vt.Int32, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int8, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField))

    val packedRecord = PackRecordLike(unpackedRecord, 16, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(thirdField, firstField, secondField),
      inline=true
    ))
  }

  test("three fields that cannot fit") {
    val firstField = new vt.RecordField("first", vt.Int64, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int8, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int64, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField))

    val packedRecord = PackRecordLike(unpackedRecord, 16, targetPlatform)

    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(firstField, thirdField, secondField),
      inline=false
    ))
  }

  test("three fields that can be reordered to fit") {
    val firstField = new vt.RecordField("first", vt.Int32, mutable=false)
    val secondField = new vt.RecordField("second", vt.Int64, mutable=false)
    val thirdField = new vt.RecordField("third", vt.Int8, mutable=false)
    // Have these extra fields to ensure their relative order is preserved
    // This is important to generate stable LLVM IR
    val fourthField = new vt.RecordField("fourth", vt.Int8, mutable=false)
    val fifthField = new vt.RecordField("fifth", vt.Int8, mutable=false)
    val sixthField = new vt.RecordField("sixth", vt.Int8, mutable=false)
    val unpackedRecord = new vt.RecordType("<test>", List(firstField, secondField, thirdField, fourthField, fifthField, sixthField))

    val packedRecord = PackRecordLike(unpackedRecord, 16, targetPlatform)

    // This assumes that we repack for space minimization
    assert(packedRecord === PackRecordLike.PackedRecordLike(
      fieldOrder=List(secondField, firstField, thirdField, fourthField, fifthField, sixthField),
      inline=true
    ))
  }
}
