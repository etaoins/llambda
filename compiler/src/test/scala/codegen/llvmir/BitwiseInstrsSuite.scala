package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException
import org.scalatest.FunSuite

class BitwiseInstrsSuite extends FunSuite {
  test("trivial and") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(32), 1) 

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = and("trivial")(op1, op2)
      
      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%trivial1 = and i32 12, 1")
  }
  
  test("and of different int sizes") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = and("error")(op1, op2)
      }
    }
  }
  
  test("and of non-ints") {
    val op1 = DoubleConstant(12) 
    val op2 = DoubleConstant(1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = and("error")(op1, op2)
      }
    }
  }
  
  test("trivial or") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(32), 1) 

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = or("trivial")(op1, op2)
      
      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%trivial1 = or i32 12, 1")
  }
  
  test("or of different int sizes") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = or("error")(op1, op2)
      }
    }
  }
  
  test("or of non-ints") {
    val op1 = DoubleConstant(12) 
    val op2 = DoubleConstant(1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = or("error")(op1, op2)
      }
    }
  }
  
  test("trivial xor") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(32), 1) 

    val block = new IrBlockBuilder()(new LocalNameSource) {
      val resultVar = xor("trivial")(op1, op2)
      
      assert(resultVar.irType === IntegerType(32))
    }

    assert(block.toIr === "\t%trivial1 = xor i32 12, 1")
  }
  
  test("xor of different int sizes") {
    val op1 = IntegerConstant(IntegerType(32), 12) 
    val op2 = IntegerConstant(IntegerType(64), 1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = xor("error")(op1, op2)
      }
    }
  }
  
  test("xor of non-ints") {
    val op1 = DoubleConstant(12) 
    val op2 = DoubleConstant(1) 

    new IrBlockBuilder()(new LocalNameSource) {
      intercept[InternalCompilerErrorException] {
        val resultVar = xor("error")(op1, op2)
      }
    }
  }
}
