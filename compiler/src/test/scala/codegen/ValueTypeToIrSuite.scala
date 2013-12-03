package llambda.codegen

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}
import org.scalatest.FunSuite

class ValueTypeToIrSuite extends FunSuite {
  test("bool") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.CBool))

    assert(typeWithSign.irType === llvmir.IntegerType(8))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("int8") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Int8))
    
    assert(typeWithSign.irType === llvmir.IntegerType(8))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int16") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Int16))

    assert(typeWithSign.irType === llvmir.IntegerType(16))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int32") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Int32))

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int64") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Int64))

    assert(typeWithSign.irType === llvmir.IntegerType(64))
    assert(typeWithSign.signed === Some(true))
  }

  test("uint8") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.UInt8))

    assert(typeWithSign.irType === llvmir.IntegerType(8))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("uint16") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.UInt16))

    assert(typeWithSign.irType === llvmir.IntegerType(16))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("uint32") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.UInt32))

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("float") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Float))

    assert(typeWithSign.irType === llvmir.FloatType)
    assert(typeWithSign.signed === None)
  }
  
  test("double") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Double))

    assert(typeWithSign.irType === llvmir.DoubleType)
    assert(typeWithSign.signed === None)
  }
  
  test("boxed datum") {
    val typeWithSign = ValueTypeToIr(vt.BoxedIntrinsicType(bt.BoxedDatum))

    assert(typeWithSign.irType === llvmir.PointerType(bt.BoxedDatum.irType))
    assert(typeWithSign.signed === None)
  }
  
  test("boxed boolean") {
    val typeWithSign = ValueTypeToIr(vt.BoxedIntrinsicType(bt.BoxedBoolean))

    assert(typeWithSign.irType === llvmir.PointerType(bt.BoxedBoolean.irType))
    assert(typeWithSign.signed === None)
  }

  test("utf8 string") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.Utf8CString))

    assert(typeWithSign.irType === llvmir.PointerType(llvmir.IntegerType(8)))
    assert(typeWithSign.signed === None)
  }
  
  test("unicode char") {
    val typeWithSign = ValueTypeToIr(vt.ScalarType(nfi.UnicodeChar))

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("boxed record") {
    val recordType = new vt.BoxedRecordType("recordType", Nil)

    val typeWithSign = ValueTypeToIr(recordType)

    assert(typeWithSign.irType === llvmir.PointerType(bt.BoxedRecord.irType))
    assert(typeWithSign.signed === None)
  }
}
