package llambda.codegen.llvmir

import org.scalatest.FunSuite

class IrValueSuite extends FunSuite {
  test("boolean constant") {
    assert(TrueConstant.toIr === "true")
    assert(TrueConstant.irType === IntegerType(1))
    
    assert(FalseConstant.toIr === "false")
    assert(FalseConstant.irType === IntegerType(1))
  }
  
  test("integer constant") {
    assert(IntegerConstant(IntegerType(16), 45).toIr === "45")
  }

  test("single constant") {
    assert(SingleConstant(0.0f).irType === SingleType)
    assert(SingleConstant(20.0f).toIr === "20.0")
    assert(SingleConstant(-300.2f).toIr === "-300.2")
    assert(SingleConstant(Float.PositiveInfinity).toIr === "0x7f800000")
    assert(SingleConstant(Float.NegativeInfinity).toIr === "0xff800000")
    assert(SingleConstant(Float.NaN).toIr === "0x7fc00000")
  }
  
  test("double constant") {
    assert(DoubleConstant(0.0).irType === DoubleType)
    assert(DoubleConstant(20.0).toIr === "20.0")
    assert(DoubleConstant(-40000.14).toIr === "-40000.14")
    assert(DoubleConstant(Double.PositiveInfinity).toIr === "0x7ff0000000000000")
    assert(DoubleConstant(Double.NegativeInfinity).toIr === "0xfff0000000000000")
    assert(DoubleConstant(Double.NaN).toIr === "0x7ff8000000000000")
  }
  
  test("null pointer constant") {
    assert(NullPointerConstant(PointerType(IntegerType(8))).toIr === "null")
  }

  test("structure constant") {
    val testConstant = StructureConstant(List(FalseConstant, IntegerConstant(IntegerType(32), 5), NullPointerConstant(PointerType(IntegerType(32)))))

    assert(testConstant.irType === StructureType(List(IntegerType(1), IntegerType(32), PointerType(IntegerType(32)))))
    assert(testConstant.toIr === "{i1 false, i32 5, i32* null}")
  }
  
  test("array constant") {
    val testConstant = ArrayConstant(SingleType, List(SingleConstant(1), SingleConstant(1), SingleConstant(2), SingleConstant(3), SingleConstant(5)))

    assert(testConstant.irType === ArrayType(5, SingleType))
    assert(testConstant.toIr === "[float 1.0, float 1.0, float 2.0, float 3.0, float 5.0]")
  }
}
