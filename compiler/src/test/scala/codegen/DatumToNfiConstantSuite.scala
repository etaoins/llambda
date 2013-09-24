package llambda.codegen

import llambda.{ast,nfi}
import org.scalatest.{FunSuite,Inside}

class DatumToNfiConstantSuite extends FunSuite with Inside {
  test("boolean values") {
    val nfiTrue = llvmir.IntegerConstant(llvmir.IntegerType(1), 1)
    val nfiFalse = llvmir.IntegerConstant(llvmir.IntegerType(1), 0)

    assert(DatumToNfiConstant(ast.TrueLiteral, nfi.Bool) === Some(nfiTrue))
    assert(DatumToNfiConstant(ast.FalseLiteral, nfi.Bool) === Some(nfiFalse))
    assert(DatumToNfiConstant(ast.StringLiteral("HELLO"), nfi.Bool) === Some(nfiFalse))
    assert(DatumToNfiConstant(ast.IntegerLiteral(1), nfi.Bool) === Some(nfiFalse))
    assert(DatumToNfiConstant(ast.IntegerLiteral(1), nfi.Bool) === Some(nfiFalse))
  }

  test("integer values") {
    assert(DatumToNfiConstant(ast.IntegerLiteral(4), nfi.Int8) === Some(llvmir.IntegerConstant(llvmir.IntegerType(8), 4)))
    assert(DatumToNfiConstant(ast.IntegerLiteral(4), nfi.Int16) === Some(llvmir.IntegerConstant(llvmir.IntegerType(16), 4)))
    assert(DatumToNfiConstant(ast.IntegerLiteral(4), nfi.Int32) === Some(llvmir.IntegerConstant(llvmir.IntegerType(32), 4)))
    assert(DatumToNfiConstant(ast.IntegerLiteral(4), nfi.Int64) === Some(llvmir.IntegerConstant(llvmir.IntegerType(64), 4)))
  }

  test("single values") {
    assert(DatumToNfiConstant(ast.IntegerLiteral(5), nfi.Float) === Some(llvmir.SingleConstant(5.0f)))
    assert(DatumToNfiConstant(ast.RealLiteral(5.0), nfi.Float) === Some(llvmir.SingleConstant(5.0f)))
    assert(DatumToNfiConstant(ast.PositiveInfinityLiteral, nfi.Float) === Some(llvmir.SingleConstant(Float.PositiveInfinity)))
    assert(DatumToNfiConstant(ast.NegativeInfinityLiteral, nfi.Float) === Some(llvmir.SingleConstant(Float.NegativeInfinity)))

    // NaN != NaN - tricky, tricky
    inside(DatumToNfiConstant(ast.NaNLiteral, nfi.Float)) {
      case Some(llvmir.SingleConstant(value)) =>
        assert(value.isNaN)
    }
  }
  
  test("double values") {
    assert(DatumToNfiConstant(ast.IntegerLiteral(5), nfi.Double) === Some(llvmir.DoubleConstant(5.0f)))
    assert(DatumToNfiConstant(ast.RealLiteral(5.0), nfi.Double) === Some(llvmir.DoubleConstant(5.0)))
    assert(DatumToNfiConstant(ast.PositiveInfinityLiteral, nfi.Double) === Some(llvmir.DoubleConstant(Double.PositiveInfinity)))
    assert(DatumToNfiConstant(ast.NegativeInfinityLiteral, nfi.Double) === Some(llvmir.DoubleConstant(Double.NegativeInfinity)))
    
    inside(DatumToNfiConstant(ast.NaNLiteral, nfi.Double)) {
      case Some(llvmir.DoubleConstant(value)) =>
        assert(value.isNaN)
    }
  }

  test("string values") {
    assert(DatumToNfiConstant(ast.StringLiteral("Hello, world!"), nfi.Utf8String) === Some(llvmir.StringConstant("Hello, world!")))
  }

  test("unicode char values") {
    assert(DatumToNfiConstant(ast.CharLiteral(0x03bb), nfi.UnicodeChar) === Some(llvmir.IntegerConstant(llvmir.IntegerType(32), 0x03bb)))
  }

  test("nonsense conversions") {
    // These are just some random points. They are far from exhaustive
    assert(DatumToNfiConstant(ast.StringLiteral("Hello, world!"), nfi.Int8) === None)
    assert(DatumToNfiConstant(ast.IntegerLiteral(5), nfi.Utf8String) === None)
    assert(DatumToNfiConstant(ast.EmptyList, nfi.Float) === None)
  }
}
