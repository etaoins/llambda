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
  
  test("unnamed_attr decl") {
    assert(IrFunctionDecl(
        result=IrFunction.Result(VoidType, Set()), 
        name="funcname",
        arguments=Nil,
        unnamedAddr=true
      ).toIr === "declare unnamed_addr void @funcname()")
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
      ).toIr === "declare externally_available protected coldcc unnamed_addr zeroext i32 @superfunc(i8* noalias nocapture, [40 x i32] zeroext) cold nounwind readnone readonly gc \"shadow\"")
  }

  test("empty function def") {
    val result = IrFunction.Result(VoidType, Set())
   
    val function = new IrFunctionDef(
      result=result,
      name="donothing",
      namedArguments=Nil) {

      addBlock("entry")(new IrBlock {
        retVoid()
      })
    }

    assert(function.toIr ===
      "define void @donothing() {\n" +
      "entry1:\n" +
      "\tret void\n" +
      "}")
  }

  test("function returning arg def") {
    val result = Result(IntegerType(32), Set())
    
    val namedArguments = List("testArg" -> Argument(IntegerType(32)))
   
    val function = new IrFunctionDef(
      result=result,
      name="retArg",
      namedArguments=namedArguments) {

      addBlock("entry")(new IrBlock {
        ret(argumentValues("testArg"))
      })
    }

    assert(function.toIr ===
      "define i32 @retArg(i32 %testArg) {\n" +
      "entry1:\n" +
      "\tret i32 %testArg\n" +
      "}")
  }

  test("hello world def") {
    val helloWorldDef = IrGlobalVariableDef(
      name="helloWorldString",
      initializer=StringConstant("Hello, world!"),
      constant=true,
      unnamedAddr=true)
    
    val putsDecl = {
      IrFunctionDecl(
        result=IrFunction.Result(IntegerType(32), Set()),
        arguments=List(IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture))),
        name="puts",
        attributes=Set(IrFunction.NoUnwind))
    }

    val result = IrFunction.Result(IntegerType(32), Set())
    
    val namedArguments = List(
      "argc" -> IrFunction.Argument(IntegerType(32)),
      "argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8)))))

    val function = new IrFunctionDef(
      result=result,
      namedArguments=namedArguments,
      name="main") {
      
      addBlock("entry")(new IrBlock {
        val helloPointer = getelementptr("helloPtr")(
          resultType=PointerType(IntegerType(8)),
          basePointer=helloWorldDef.variable,
          indices=List(0, 0)
        )
          
        callDecl(None)(putsDecl, helloPointer :: Nil)
        ret(IntegerConstant(IntegerType(32), 0))
      })
    }

    assert(function.toIr === 
      """|define i32 @main(i32 %argc, i8** %argv) {
         |entry1:
         |	%helloPtr1 = getelementptr [14 x i8]* @helloWorldString, i32 0, i32 0
         |	call i32 @puts(i8* %helloPtr1) nounwind
         |	ret i32 0
         |}""".stripMargin)
  }
  
  test("multi block function def") {
    val result = IrFunction.Result(VoidType, Set())
   
    val function = new IrFunctionDef(
      result=result,
      name="donothing",
      namedArguments=Nil) {

      val continueLabel = declareBlock("continue")

      addBlock("entry")(new IrBlock {
        uncondBranch(continueLabel)
      })

      defineBlock(continueLabel)(new IrBlock {
        retVoid()
      })
    }

    assert(function.toIr ===
      "define void @donothing() {\n" +
      "entry1:\n" +
      "\tbr label %continue1\n" +
      "continue1:\n" +
      "\tret void\n" +
      "}")
  }

}

