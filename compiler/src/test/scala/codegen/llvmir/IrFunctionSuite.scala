package llambda.codegen.llvmir

import org.scalatest.FunSuite
import IrFunction._

class IrFunctionSuite extends FunSuite {
  test("trivial void function decl") {
    val result = IrFunction.Result(VoidType, Set())

    assert(IrFunctionDecl(result, "funcname", Nil).toIr === "declare void @funcname()")
  }
  
  test("puts function decl") {
    val result = IrFunction.Result(IntegerType(32), Set())
    val arguments = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture)))

    assert(IrFunctionDecl(result, "puts", arguments).toIr === "declare i32 @puts(i8* nocapture)")
  }
  
  test("puts function decl to type") {
    val result = IrFunction.Result(IntegerType(32), Set())
    val arguments = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture)))

    assert(IrFunctionDecl(result, "puts", arguments).irType === FunctionType(IntegerType(32), List(PointerType(IntegerType(8)))))
  }
  
  test("puts function decl to value") {
    val result = IrFunction.Result(IntegerType(32), Set())
    val arguments = List(IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture)))
    val irValue = IrFunctionDecl(result, "puts", arguments).irValue

    assert(irValue.isInstanceOf[GlobalVariable])
    assert(irValue.irType === PointerType(FunctionType(IntegerType(32), List(PointerType(IntegerType(8))))))
    assert(irValue.toIr === "@puts")
  }

  test("signext return decl") {
    val result = IrFunction.Result(IntegerType(8), Set(SignExt))
    
    assert(IrFunctionDecl(result, "returns_signed_char", Nil).toIr === "declare signext i8 @returns_signed_char()") 
  }
  
  test("attribute decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        attributes=Set(IrFunction.Cold)
      ).toIr === "declare void @funcname() cold")
  }
  
  test("linkage decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        linkage=Linkage.Private
      ).toIr === "declare private void @funcname()")
  }
  
  test("visibility decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        visibility=Visibility.Hidden
      ).toIr === "declare hidden void @funcname()")
  }
  
  test("calling conv decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        callingConv=CallingConv.FastCC
      ).toIr === "declare fastcc void @funcname()")
  }
  
  test("escaped identiifer decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="Function Name",
        arguments=Nil
      ).toIr === "declare void @\"Function Name\"()")
  }
  
  test("unnamed_addr decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        unnamedAddr=true
      ).toIr === "declare void @funcname() unnamed_addr")
  }

  test("garabge collector decl") {
    val result = IrFunction.Result(VoidType, Set())

    assert(IrFunctionDecl(
        result=result,
        name="f",
        arguments=Nil,
        gc=Some("shadow")
      ).toIr === "declare void @f() gc \"shadow\"")
  }

  test("christmas tree decl") {
    val result = IrFunction.Result(IntegerType(32), Set(ZeroExt))
    val arguments = IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture, NoAlias)) ::
                    IrFunction.Argument(ArrayType(40, IntegerType(32)), Set(ZeroExt)) :: 
                    Nil

    assert(IrFunctionDecl(
        result=result,
        name="superfunc",
        arguments=arguments,
        gc=Some("shadow"),
        callingConv=CallingConv.ColdCC,
        visibility=Visibility.Protected,
        unnamedAddr=true,
        attributes=Set(IrFunction.Cold, IrFunction.NoUnwind, IrFunction.ReadNone, IrFunction.ReadOnly),
        linkage=Linkage.ExternallyAvailable
      ).toIr === "declare externally_available protected coldcc zeroext i32 @superfunc(i8* noalias nocapture, [40 x i32] zeroext) unnamed_addr cold nounwind readnone readonly gc \"shadow\"")
  }
}
