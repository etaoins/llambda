package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.InternalCompilerErrorException

import org.scalatest.FunSuite

class CellTypeSuite extends llvmir.IrTestSuite {
  test("create constant inexact rational cell") {
    val innerValue = llvmir.DoubleConstant(31.5)

    val constant = ct.InexactRationalCell.createConstant(innerValue)
    val expectedTypeId = ct.InexactRationalCell.typeId.toString

    assert(constant.toIr === 
      s"{%numeric {%datum {i8 ${expectedTypeId}, i8 0}}, double 31.5}"
    )
  }
  
  test("create constant with incorrect type") {
    val innerValue = llvmir.StringConstant("Hello, world!")
    
    intercept[InternalCompilerErrorException] {
      ct.InexactRationalCell.createConstant(innerValue)
    }
  }

  test("isTypeOrSubtypeOf") {
    assert(ct.InexactRationalCell.isTypeOrSubtypeOf(ct.InexactRationalCell) === true)
    assert(ct.InexactRationalCell.isTypeOrSubtypeOf(ct.NumericCell) === true)
    assert(ct.InexactRationalCell.isTypeOrSubtypeOf(ct.DatumCell) === true)
    
    assert(ct.InexactRationalCell.isTypeOrSubtypeOf(ct.StringCell) === false)
    assert(ct.NumericCell.isTypeOrSubtypeOf(ct.InexactRationalCell) === false)
    assert(ct.DatumCell.isTypeOrSubtypeOf(ct.InexactRationalCell) === false)
  }
  
  test("isTypeOrSupertypeOf") {
    assert(ct.InexactRationalCell.isTypeOrSupertypeOf(ct.InexactRationalCell) === true)
    assert(ct.NumericCell.isTypeOrSupertypeOf(ct.InexactRationalCell) === true)
    assert(ct.DatumCell.isTypeOrSupertypeOf(ct.InexactRationalCell) === true)
    
    assert(ct.StringCell.isTypeOrSupertypeOf(ct.InexactRationalCell) === false)
    assert(ct.InexactRationalCell.isTypeOrSupertypeOf(ct.NumericCell) === false)
    assert(ct.InexactRationalCell.isTypeOrSupertypeOf(ct.DatumCell) === false)
  }

  test("concreteTypes") {
    assert(ct.ListElementCell.concreteTypes === Set(ct.PairCell, ct.EmptyListCell))
    assert(ct.NumericCell.concreteTypes === Set(ct.ExactIntegerCell, ct.InexactRationalCell))
    assert(ct.StringCell.concreteTypes === Set(ct.StringCell))
  }
  
  test("noop bitcast") {
    val nullNumeric = llvmir.NullPointerConstant(llvmir.PointerType(ct.NumericCell.irType))

    val block = createTestBlock()
    val resultValue = ct.NumericCell.genPointerBitcast(block)(nullNumeric)

    assert(resultValue === nullNumeric)
  }
  
  test("simple bitcast") {
    val nullNumeric = llvmir.NullPointerConstant(llvmir.PointerType(ct.NumericCell.irType))

    val block = createTestBlock()
    val resultValue = ct.DatumCell.genPointerBitcast(block)(nullNumeric)

    assertInstr(block, "%datumCast1 = bitcast %numeric* null to %datum*") 

    assert(resultValue != nullNumeric)
    assert(resultValue.irType === llvmir.PointerType(ct.DatumCell.irType))
  }
}
