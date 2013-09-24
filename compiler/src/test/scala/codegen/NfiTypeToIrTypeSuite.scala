package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}
import org.scalatest.FunSuite

class NfiTypeToIrTypeSuite extends FunSuite {
  test("bool32") {
    val typeWithSign = NfiTypeToIrType(nfi.Bool)

    assert(typeWithSign.irType === llvmir.IntegerType(1))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("int8") {
    val typeWithSign = NfiTypeToIrType(nfi.Int8)
    
    assert(typeWithSign.irType === llvmir.IntegerType(8))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int16") {
    val typeWithSign = NfiTypeToIrType(nfi.Int16)

    assert(typeWithSign.irType === llvmir.IntegerType(16))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int32") {
    val typeWithSign = NfiTypeToIrType(nfi.Int32)

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(true))
  }
  
  test("int64") {
    val typeWithSign = NfiTypeToIrType(nfi.Int64)

    assert(typeWithSign.irType === llvmir.IntegerType(64))
    assert(typeWithSign.signed === Some(true))
  }

  test("uint8") {
    val typeWithSign = NfiTypeToIrType(nfi.UInt8)

    assert(typeWithSign.irType === llvmir.IntegerType(8))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("uint16") {
    val typeWithSign = NfiTypeToIrType(nfi.UInt16)

    assert(typeWithSign.irType === llvmir.IntegerType(16))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("uint32") {
    val typeWithSign = NfiTypeToIrType(nfi.UInt32)

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(false))
  }
  
  test("uint64") {
    val typeWithSign = NfiTypeToIrType(nfi.UInt64)

    assert(typeWithSign.irType === llvmir.IntegerType(64))
    assert(typeWithSign.signed === Some(false))
  }

  test("float") {
    val typeWithSign = NfiTypeToIrType(nfi.Float)

    assert(typeWithSign.irType === llvmir.SingleType)
    assert(typeWithSign.signed === None)
  }
  
  test("double") {
    val typeWithSign = NfiTypeToIrType(nfi.Double)

    assert(typeWithSign.irType === llvmir.DoubleType)
    assert(typeWithSign.signed === None)
  }
  
  test("boxed datum") {
    val typeWithSign = NfiTypeToIrType(nfi.BoxedDatum)

    assert(typeWithSign.irType === bt.BoxedDatum.irType)
    assert(typeWithSign.signed === None)
  }

  test("utf8 string") {
    val typeWithSign = NfiTypeToIrType(nfi.Utf8String)

    assert(typeWithSign.irType === llvmir.PointerType(llvmir.IntegerType(8)))
    assert(typeWithSign.signed === None)
  }
  
  test("unicode char") {
    val typeWithSign = NfiTypeToIrType(nfi.UnicodeChar)

    assert(typeWithSign.irType === llvmir.IntegerType(32))
    assert(typeWithSign.signed === Some(true))
  }
}
