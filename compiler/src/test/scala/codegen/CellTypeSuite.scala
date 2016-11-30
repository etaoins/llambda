package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir
import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException

import org.scalatest.FunSuite

class CellTypeSuite extends FunSuite {
  private def createTestBlock() =
    (new llvmir.IrFunctionBuilder(
      new llvmir.IrModuleBuilder,
      llvmir.IrFunction.Result(llvmir.VoidType), 
      "dontcare", 
      Nil)
    ).entryBlock

  test("create constant flonum cell") {
    val innerValue = llvmir.DoubleConstant(31.5)

    val constant = ct.FlonumCell.createConstant(innerValue)
    val expectedTypeId = ct.FlonumCell.typeId.toString

    assert(constant.toIr === 
      s"{%number {%any {i8 ${expectedTypeId}, i8 1}}, double 31.5}"
    )
  }
  
  test("create constant with incorrect type") {
    val innerValue = llvmir.StringConstant.fromUtf8String("Hello, world!")
    
    intercept[InternalCompilerErrorException] {
      ct.FlonumCell.createConstant(innerValue)
    }
  }

  test("concreteTypes") {
    assert(ct.ListElementCell.concreteTypes === Set(ct.PairCell, ct.EmptyListCell))
    assert(ct.NumberCell.concreteTypes === Set(ct.IntegerCell, ct.FlonumCell))
    assert(ct.StringCell.concreteTypes === Set(ct.StringCell))
  }
  
  test("noop bitcast") {
    val nullNumber = llvmir.NullPointerConstant(llvmir.PointerType(ct.NumberCell.irType))

    val block = createTestBlock()
    val resultValue = ct.NumberCell.genPointerBitcast(block)(nullNumber)

    assert(resultValue === nullNumber)
  }
  
  test("simple bitcast") {
    val nullNumber = llvmir.NullPointerConstant(llvmir.PointerType(ct.NumberCell.irType))

    val block = createTestBlock()
    val resultValue = ct.AnyCell.genPointerBitcast(block)(nullNumber)

    assert(block.toIr === "entry:\n\t%anyCast1 = bitcast %number* null to %any*") 

    assert(resultValue != nullNumber)
    assert(resultValue.irType === llvmir.PointerType(ct.AnyCell.irType))
  }
}
