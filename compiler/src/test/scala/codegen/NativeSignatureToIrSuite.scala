package llambda.codegen

import org.scalatest.FunSuite

import llambda.et.NativeFunction
import llambda.nfi

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

class NativeSignatureToIrSuite extends FunSuite {
  test("argless void function") {
    val testNativeFunc = NativeFunction(
      fixedArgs=Nil,
      hasRestArg=false,
      returnType=None,
      nativeSymbol="lliby_test")

    val irSignature = NativeSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=Nil
    ))
  }
  
  test("function taking UTF-8 string, unsigned int returning signed int") {
    val testNativeFunc = NativeFunction(
      fixedArgs=nfi.Utf8CString :: nfi.UInt16 :: Nil,
      hasRestArg=false,
      returnType=Some(nfi.Int32),
      nativeSymbol="lliby_test")

    val irSignature = NativeSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(SignExt)),
      arguments=Argument(PointerType(IntegerType(8))) :: Argument(IntegerType(16), Set(ZeroExt)) :: Nil
    ))
  }
  
  test("function taking only rest args returning unsigned int") {
    val testNativeFunc = NativeFunction(
      fixedArgs=Nil,
      hasRestArg=true,
      returnType=Some(nfi.UInt32),
      nativeSymbol="lliby_test")

    val irSignature = NativeSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=Argument(PointerType(bt.BoxedListElement.irType)) :: Nil
    ))
  }
  
  test("function taking two numerics, rest arg returning rational") {
    val testNativeFunc = NativeFunction(
      fixedArgs=nfi.BoxedValue(bt.BoxedNumeric) :: nfi.BoxedValue(bt.BoxedNumeric) :: Nil,
      hasRestArg=true,
      returnType=Some(nfi.BoxedValue(bt.BoxedInexactRational)),
      nativeSymbol="lliby_test")

    val irSignature = NativeSignatureToIr(testNativeFunc)

    assert(irSignature === IrSignature(
      result=Result(PointerType(bt.BoxedInexactRational.irType)),
      arguments=List(
        Argument(PointerType(bt.BoxedNumeric.irType)),
        Argument(PointerType(bt.BoxedNumeric.irType)), 
        Argument(PointerType(bt.BoxedListElement.irType))
      )
    ))
  }

  test("boxed procedure signature") {
    val irSignature = NativeSignatureToIr(BoxedProcedureSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(bt.BoxedDatum.irType)),
      arguments=List(
        Argument(PointerType(bt.BoxedProcedure.irType)),
        Argument(PointerType(bt.BoxedListElement.irType))
      )
    ))
  }
}