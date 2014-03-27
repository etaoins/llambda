package io.llambda.typegen

import org.scalatest.FunSuite
import io.llambda.llvmir

class LlvmTypeToScalaConstructorSuite extends FunSuite {
  test("integer type") {
    assert(LlvmTypeToScalaConstructor(llvmir.IntegerType(16)) ===
      """IntegerType(16)"""
    )
  }
  
  test("double type") {
    assert(LlvmTypeToScalaConstructor(llvmir.DoubleType) ===
      """DoubleType"""
    )
  }
  
  test("array type") {
    assert(LlvmTypeToScalaConstructor(llvmir.ArrayType(5, llvmir.DoubleType)) ===
      """ArrayType(5, DoubleType)"""
    )
  }
  
  test("multidimensional array type") {
    assert(LlvmTypeToScalaConstructor(llvmir.ArrayType(15, llvmir.ArrayType(5, llvmir.DoubleType))) ===
      """ArrayType(15, ArrayType(5, DoubleType))"""
    )
  }
  
  test("float type") {
    assert(LlvmTypeToScalaConstructor(llvmir.FloatType) ===
      """FloatType"""
    )
  }

  test("void type") {
    assert(LlvmTypeToScalaConstructor(llvmir.VoidType) ===
      """VoidType"""
    )
  }
  
  test("pointer to integer") {
    assert(LlvmTypeToScalaConstructor(llvmir.PointerType(llvmir.IntegerType(8))) ===
      """PointerType(IntegerType(8))"""
    )
  }

  test("user defined type") {
    assert(LlvmTypeToScalaConstructor(llvmir.UserDefinedType("notquoted")) ===
      """UserDefinedType("notquoted")"""
    )
    
    assert(LlvmTypeToScalaConstructor(llvmir.UserDefinedType("""Is "Quoted" \ backslash""")) ===
      """UserDefinedType("Is \"Quoted\" \\ backslash")"""
    )
  }
  
  test("function pointer") {
    assert(LlvmTypeToScalaConstructor(
      llvmir.PointerType(
        llvmir.FunctionType(
          llvmir.IntegerType(8),
          List(
            llvmir.IntegerType(64),
            llvmir.PointerType(llvmir.DoubleType)
          )
        )
      )) === 
      """PointerType(FunctionType(IntegerType(8), List(IntegerType(64), PointerType(DoubleType)), false))"""
    )
  }
}
