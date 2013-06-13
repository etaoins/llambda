package llambda.codegen.llvmir

import org.scalatest.FunSuite

class IrTypeSuite extends FunSuite {
  val i1 = IntegerType(1)
  val i16 = IntegerType(16)
  val i32 = IntegerType(32)

  test("integer types") {
    assert(i1.toIr === "i1")
    assert(i32.toIr === "i32")
    assert(IntegerType(31337).toIr === "i31337")
  }

  test("single type") {
    assert(SingleType.toIr === "float")
  }
  
  test("double type") {
    assert(DoubleType.toIr === "double")
  }

  test("void type") {
    assert(VoidType.toIr === "void")
  }

  test("array type") {
    assert(ArrayType(40, i32).toIr === "[40 x i32]")
    assert(ArrayType(4, IntegerType(8)).toIr === "[4 x i8]")
    assert(ArrayType(12, ArrayType(10, SingleType)).toIr === "[12 x [10 x float]]")
  }

  test("function type") {
    assert(FunctionType(i32, List(i32)).toIr === "i32 (i32)")
    assert(PointerType(FunctionType(SingleType, List(i16, PointerType(i32)))).toIr === "float (i16, i32*) *")
    assert(FunctionType(StructureType(List(i32, i32)), List(i32)).toIr === "{i32, i32} (i32)")
  }

  test("structure type") {
    assert(StructureType(List(i32, i32, i32)).toIr === "{i32, i32, i32}")
    assert(StructureType(List(SingleType, PointerType(FunctionType(i32, List(i32))))).toIr === "{float, i32 (i32) *}")
  }

  test("pointer type") {
    assert(PointerType(ArrayType(4, i32)).toIr === "[4 x i32]*")
  }
}
