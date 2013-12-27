package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.et.NativeFunction

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.llvmir._
import llambda.llvmir.IrFunction._

class ProcedureSignatureToIrSuite extends FunSuite {
  test("argless void function") {
    val testNativeFunc = NativeFunction(
      fixedArgs=Nil,
      hasRestArg=false,
      returnType=None,
      nativeSymbol="lliby_test")

    val irSignature = ProcedureSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=Nil
    ))
  }
  
  test("function taking boolean, unsigned int returning signed int") {
    val testNativeFunc = NativeFunction(
      fixedArgs=vt.CBool :: vt.UInt16 :: Nil,
      hasRestArg=false,
      returnType=Some(vt.Int32),
      nativeSymbol="lliby_test")

    val irSignature = ProcedureSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(SignExt)),
      arguments=Argument(IntegerType(8), Set(ZeroExt)) :: Argument(IntegerType(16), Set(ZeroExt)) :: Nil
    ))
  }
  
  test("function taking only rest args returning unsigned int") {
    val testNativeFunc = NativeFunction(
      fixedArgs=Nil,
      hasRestArg=true,
      returnType=Some(vt.UInt32),
      nativeSymbol="lliby_test")

    val irSignature = ProcedureSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=Argument(PointerType(ct.ListElementCell.irType)) :: Nil
    ))
  }
  
  test("function taking two numerics, rest arg returning rational") {
    val testNativeFunc = NativeFunction(
      fixedArgs=vt.IntrinsicCellType(ct.NumericCell) :: vt.IntrinsicCellType(ct.NumericCell) :: Nil,
      hasRestArg=true,
      returnType=Some(vt.IntrinsicCellType(ct.InexactRationalCell)),
      nativeSymbol="lliby_test")

    val irSignature = ProcedureSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.InexactRationalCell.irType)),
      arguments=List(
        Argument(PointerType(ct.NumericCell.irType)),
        Argument(PointerType(ct.NumericCell.irType)), 
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }

  test("adapted procedure signature") {
    val irSignature = ProcedureSignatureToIr(AdaptedProcedureSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.DatumCell.irType)),
      arguments=List(
        Argument(PointerType(ct.ProcedureCell.irType)),
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }
}
