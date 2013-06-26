package llambda.codegen

import llambda.nfi
import org.scalatest.FunSuite

class NfiTypeToIrTypeSuite extends FunSuite {
  test("bool32") {
    assert(NfiTypeToIrType(nfi.Bool32) === llvmir.IntegerType(32))
  }
  
  test("int8") {
    assert(NfiTypeToIrType(nfi.Int8) === llvmir.IntegerType(8))
  }
  
  test("int16") {
    assert(NfiTypeToIrType(nfi.Int16) === llvmir.IntegerType(16))
  }
  
  test("int32") {
    assert(NfiTypeToIrType(nfi.Int32) === llvmir.IntegerType(32))
  }
  
  test("int64") {
    assert(NfiTypeToIrType(nfi.Int64) === llvmir.IntegerType(64))
  }

  test("uint8") {
    assert(NfiTypeToIrType(nfi.UInt8) === llvmir.IntegerType(8))
  }
  
  test("uint16") {
    assert(NfiTypeToIrType(nfi.UInt16) === llvmir.IntegerType(16))
  }
  
  test("uint32") {
    assert(NfiTypeToIrType(nfi.UInt32) === llvmir.IntegerType(32))
  }
  
  test("uint64") {
    assert(NfiTypeToIrType(nfi.UInt64) === llvmir.IntegerType(64))
  }

  test("float") {
    assert(NfiTypeToIrType(nfi.Float) === llvmir.SingleType)
  }
  
  test("double") {
    assert(NfiTypeToIrType(nfi.Double) === llvmir.DoubleType)
  }
  
  test("boxed datum") {
    assert(NfiTypeToIrType(nfi.BoxedDatum) === llvmir.PointerType(llvmir.UserDefinedType("genericExpr")))
  }

  test("utf8 string") {
    assert(NfiTypeToIrType(nfi.Utf8String) === llvmir.PointerType(llvmir.IntegerType(8)))
  }
}
