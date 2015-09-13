package io.llambda.llvmir

import org.scalatest.FunSuite
import IrFunction._

class IrModuleSuite extends FunSuite {
  test("hello world module") {
    val module = new IrModuleBuilder
    module.identifyCompiler("typegen unit tests")

    val helloWorldDef = IrGlobalVariableDef(
      name="helloWorldString",
      initializer=StringConstant.fromUtf8String("Hello, world!"),
      constant=true,
      unnamedAddr=true)
    
    val putsDecl = {
      IrFunctionDecl(
        result=IrFunction.Result(IntegerType(32), Set()),
        arguments=List(IrFunction.Argument(PointerType(IntegerType(8)), Set(NoCapture))),
        name="Put String",
        attributes=Set(IrFunction.NoUnwind))
    }

    val result = IrFunction.Result(IntegerType(32), Set())
    
    val namedArguments = List(
      "argc" -> IrFunction.Argument(IntegerType(32)),
      "escaped argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8)))))

    val mainFunction = new IrFunctionBuilder(
      module=module,
      result=result,
      namedArguments=namedArguments,
      name="main"
    )
      
    val entryBlock = mainFunction.entryBlock
    val helloPointer = entryBlock.getelementptr("helloPtr")(
      elementType=IntegerType(8),
      basePointer=helloWorldDef.variable,
      indices=List(0, 0).map(IntegerConstant(IntegerType(32), _))
    )
      
    entryBlock.callDecl(None)(putsDecl, helloPointer :: Nil)
    entryBlock.ret(IntegerConstant(IntegerType(32), 0))

    module.nameType("myInt64", IntegerType(64))
    module.nameType("needs-escape-64", IntegerType(64))

    module.numberMetadataNode(
      TbaaMetadata("root type")
    )

    module.defineGlobalVariable(helloWorldDef)
    module.declareFunction(putsDecl)
    module.defineFunction(mainFunction)

    val aliasDef = IrAliasDef(
      name="mainAlias",
      aliasee=mainFunction.irValue
    )

    module.defineAlias(aliasDef)

    // Make sure unlessDeclared works
    module.unlessDeclared(putsDecl) {
      assert(false)
    }

    // And is declared
    assert(module.isDeclared(putsDecl) === true)
    assert(module.isDeclared("unknownDecl") === false)

    var blockRun = false
    module.unlessDeclared("randomString") {
      blockRun = true
    }

    assert(blockRun)

    assert(module.toIr ===
      "%myInt64 = type i64\n" +
      "%\"needs-escape-64\" = type i64\n" +
      "!0 = !{!\"typegen unit tests\"}\n" +
      "!1 = !{!\"root type\"}\n" +
      "!llvm.ident = !{!0}\n" +
      "@helloWorldString = unnamed_addr constant [14 x i8] c\"Hello, world!\\00\"\n" + 
      "declare i32 @\"Put String\"(i8* nocapture) nounwind\n" +
      "define i32 @main(i32 %argc, i8** %\"escaped argv\") {\n" +
      "entry:\n" +
      "\t%helloPtr1 = getelementptr [14 x i8], [14 x i8]* @helloWorldString, i32 0, i32 0\n" +
      "\tcall i32 @\"Put String\"(i8* %helloPtr1) nounwind\n" +
      "\tret i32 0\n" +
      "}\n" +
      "@mainAlias = alias i32 (i32, i8**)* @main"
    )
  }
}
