package llambda.codegen

import org.scalatest.FunSuite

import llambda.et.NativeFunction
import llambda.nfi

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

class NativeSignatureToIrCallableSuite extends FunSuite {
  test("argless void function") {
    val testNativeFunc = NativeFunction(
      fixedArgs=Nil,
      hasRestArg=false,
      returnType=None,
      nativeSymbol="lliby_test")

    val irCallable = NativeSignatureToIrCallable(testNativeFunc)

    assert(irCallable === IrCallable(
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

    val irCallable = NativeSignatureToIrCallable(testNativeFunc)

    assert(irCallable === IrCallable(
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

    val irCallable = NativeSignatureToIrCallable(testNativeFunc)

    assert(irCallable === IrCallable(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=Argument(PointerType(bt.BoxedPair.irType)) :: Nil
    ))
  }
  
  test("function taking two numerics, rest arg returning rational") {
    val testNativeFunc = NativeFunction(
      fixedArgs=nfi.BoxedValue(bt.BoxedNumeric) :: nfi.BoxedValue(bt.BoxedNumeric) :: Nil,
      hasRestArg=true,
      returnType=Some(nfi.BoxedValue(bt.BoxedInexactRational)),
      nativeSymbol="lliby_test")

    val irCallable = NativeSignatureToIrCallable(testNativeFunc)

    assert(irCallable === IrCallable(
      result=Result(PointerType(bt.BoxedInexactRational.irType)),
      arguments=List(
        Argument(PointerType(bt.BoxedNumeric.irType)),
        Argument(PointerType(bt.BoxedNumeric.irType)), 
        Argument(PointerType(bt.BoxedPair.irType))
      )
    ))
  }
}
