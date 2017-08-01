package io.llambda.typegen

import org.scalatest.FunSuite
import io.llambda.llvmir

class FieldTypeToLlvmSuite extends FunSuite {
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

    assert(FieldTypeToLlvm(testType) === llvmir.IntegerType(32))
  }

  test("pointer to double") {
    val testType = PointerFieldType(doubleType)

    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(llvmir.DoubleType))
  }

  test("array of doubles") {
    val testType = ArrayFieldType(List(10), doubleType)

    assert(FieldTypeToLlvm(testType) === llvmir.ArrayType(10, llvmir.DoubleType))
  }

  test("multidimensional array of doubles") {
    val testType = ArrayFieldType(List(5, 10), doubleType)

    assert(FieldTypeToLlvm(testType) === llvmir.ArrayType(5, llvmir.ArrayType(10, llvmir.DoubleType)))
  }

  test("aliased type with explicit C++ name") {
    val aliasedType = PointerFieldType(doubleType)

    val testType = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=Some("DoublePtr"),
      needsDefinition=false
    )

    // LLVM doesn't care about the C++ name
    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(llvmir.DoubleType))
  }

  test("aliased type without C++ name") {
    val aliasedType = PointerFieldType(uint8Type)

    val testType = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=None,
      needsDefinition=false
    )

    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(llvmir.IntegerType(8)))
  }

  test("pointer to aliased type with explicit C++ name") {
    val aliasedType = PointerFieldType(doubleType)

    val typeAlias = new FieldTypeAlias(
      aliasedType=aliasedType,
      cppTypeName=Some("DoublePtr"),
      needsDefinition=false
    )

    val testType = PointerFieldType(typeAlias)

    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(llvmir.PointerType(llvmir.DoubleType)))
  }

  test("function pointer returning non-void") {
    val testType = FunctionPointerFieldType(
      Some(PointerFieldType(doubleType)),
      List(uint8Type, int32Type)
    )

    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(
      llvmir.FunctionType(
        llvmir.PointerType(llvmir.DoubleType),
        List(
          llvmir.IntegerType(8),
          llvmir.IntegerType(32)
        )
      )
    ))
  }

  test("function pointer returning void") {
    val testType = FunctionPointerFieldType(
      None,
      List(uint8Type, int32Type)
    )

    assert(FieldTypeToLlvm(testType) === llvmir.PointerType(
      llvmir.FunctionType(
        llvmir.VoidType,
        List(
          llvmir.IntegerType(8),
          llvmir.IntegerType(32)
        )
      )
    ))
  }
}
