package llambda.codegen.llvmir

import org.scalatest.FunSuite
import IrFunction._

class IrModuleSuite extends FunSuite {
  test("hello world module") {
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

    val mainFunction = new IrFunctionDef(
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

    val module = new IrModule {
      defineGlobalVariable(helloWorldDef)
      declareFunction(putsDecl)
      defineFunction(mainFunction)
    }

    assert(module.toIr ===
      "@helloWorldString = unnamed_addr constant [14 x i8] c\"Hello, world!\\00\"\n" + 
      "declare i32 @puts(i8* nocapture) nounwind\n" +
      "define i32 @main(i32, i8**) {\n" +
      "entry1:\n" +
      "\t%helloPtr1 = getelementptr [14 x i8]* @helloWorldString, i32 0, i32 0\n" +
      "\tcall i32 @puts(i8* %helloPtr1) nounwind\n" +
      "\tret i32 0\n" +
      "}"
    )
  }
}
