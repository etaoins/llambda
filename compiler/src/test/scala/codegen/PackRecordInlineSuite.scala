package llambda.codegen

import org.scalatest.FunSuite

import llambda.platform
import llambda.{celltype => ct}
import llambda.{valuetype => vt}

class PackRecordInlineSuite extends FunSuite {
  test("empty record can be packed") {
    val packedRecord = PackRecordInline(Nil, 16, platform.Posix64)

    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=Nil,
      inline=true
    ))
  }
  
  test("one field that fits is packed inline") {
    val onlyField = new vt.RecordField("only", vt.Int64) 

    val packedRecord = PackRecordInline(List(onlyField), 16, platform.Posix64)
    
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(onlyField),
      inline=true
    ))
  }
  
  test("platforms with non-natural alignment never inline") {
    // Without predictable alignment we can't inline
    val unnaturalPosix64 = new platform.TargetPlatform with platform.AbstractLP64 with platform.AbstractPosix {
      override val usesNaturalAlignment = false
    }

    val onlyField = new vt.RecordField("only", vt.Int64) 
    val packedRecord = PackRecordInline(List(onlyField), 16, unnaturalPosix64)
    
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(onlyField),
      inline=false
    ))
  }
  
  
  test("one field that does not fit is packed out-of-line") {
    val onlyField = new vt.RecordField("only", vt.Int64) 

    val packedRecord = PackRecordInline(List(onlyField), 4, platform.Posix64)
    
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(onlyField),
      inline=false
    ))
  }
  
  test("three fields that already fit have their order preserved") {
    val firstField = new vt.RecordField("first", vt.Int32) 
    val secondField = new vt.RecordField("second", vt.Int8) 
    val thirdField = new vt.RecordField("third", vt.Int64) 

    val packedRecord = PackRecordInline(List(firstField, secondField, thirdField), 16, platform.Posix64)
    
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(firstField, secondField, thirdField),
      inline=true
    ))
  }
  
  test("three fields that cannot fit have their order preserved") {
    val firstField = new vt.RecordField("first", vt.Int64) 
    val secondField = new vt.RecordField("second", vt.Int8) 
    val thirdField = new vt.RecordField("third", vt.Int64) 

    val packedRecord = PackRecordInline(List(firstField, secondField, thirdField), 16, platform.Posix64)
    
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(firstField, secondField, thirdField),
      inline=false
    ))
  }
  
  test("three fields that can be reordered to fit are packed") {
    val firstField = new vt.RecordField("first", vt.Int32) 
    val secondField = new vt.RecordField("second", vt.Int64) 
    val thirdField = new vt.RecordField("third", vt.Int8) 

    val packedRecord = PackRecordInline(List(firstField, secondField, thirdField), 16, platform.Posix64)
    
    // This assumes that we repack for space minimization
    assert(packedRecord === PackRecordInline.PackedRecord(
      fieldOrder=List(secondField, firstField, thirdField),
      inline=true
    ))
  }
}
