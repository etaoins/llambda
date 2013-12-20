package io.llambda.typegen

import org.scalatest.FunSuite
import io.llambda.llvmir

class FieldTypeToCppSuite extends FunSuite {
  private val int32Type = PrimitiveFieldType(
    Some(true),
    llvmir.IntegerType(32),
    "std::int32_t"
  )
  
  private val uint8Type = PrimitiveFieldType(
    Some(false),
    llvmir.IntegerType(8),
    "std::uint8_t"
  )

  private val doubleType = PrimitiveFieldType(
    None,
    llvmir.DoubleType,
    "double"
  )

  test("trivial integer type") {
    val testType = int32Type

    assert(FieldTypeToCpp(testType, None) === "std::int32_t")
    assert(FieldTypeToCpp(testType, Some("member")) === "std::int32_t member")
  }
  
  test("pointer to double") {
    val testType = PointerFieldType(doubleType)

    assert(FieldTypeToCpp(testType, None) === "double*")
    assert(FieldTypeToCpp(testType, Some("member")) === "double* member")
  }
  
  test("aliased type with explicit C++ name") {
    val aliasedType = PointerFieldType(doubleType)

    val testType = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=Some("DoublePtr"),
      needsDefinition=false
    )

    assert(FieldTypeToCpp(testType, None) === "DoublePtr")
    assert(FieldTypeToCpp(testType, Some("member")) === "DoublePtr member")
  }
  
  test("aliased type without C++ name") {
    val aliasedType = PointerFieldType(uint8Type)

    val testType = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=None,
      needsDefinition=false
    )

    assert(FieldTypeToCpp(testType, None) === "std::uint8_t*")
    assert(FieldTypeToCpp(testType, Some("member")) === "std::uint8_t* member")
  }
  
  test("pointer to aliased type with explicit C++ name") {
    val aliasedType = PointerFieldType(doubleType)

    val typeAlias = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=Some("DoublePtr"),
      needsDefinition=false
    )

    val testType = PointerFieldType(typeAlias)

    assert(FieldTypeToCpp(testType, None) === "DoublePtr*")
    assert(FieldTypeToCpp(testType, Some("member")) === "DoublePtr* member")
  }
  
  test("function pointer returning non-void") {
    val testType = FunctionPointerFieldType(
      Some(PointerFieldType(doubleType)),
      List(uint8Type, int32Type)
    )

    assert(FieldTypeToCpp(testType, None) === "double* (*)(std::uint8_t, std::int32_t)")
    assert(FieldTypeToCpp(testType, Some("member")) === "double* (*member)(std::uint8_t, std::int32_t)")
  }
  
  test("function pointer returning void") {
    val testType = FunctionPointerFieldType(
      None,
      List(uint8Type, int32Type)
    )

    assert(FieldTypeToCpp(testType, None) === "void (*)(std::uint8_t, std::int32_t)")
    assert(FieldTypeToCpp(testType, Some("member")) === "void (*member)(std::uint8_t, std::int32_t)")
  }
}
