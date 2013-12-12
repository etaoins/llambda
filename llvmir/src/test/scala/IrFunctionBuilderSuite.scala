package io.llambda.llvmir

import org.scalatest.FunSuite
import IrFunction._

class IrFunctionBuilderSuite extends FunSuite {
  test("empty function def") {
    val result = IrFunction.Result(VoidType, Set())
   
    val function = new IrFunctionBuilder(
      result=result,
      name="donothing",
      namedArguments=Nil)

    function.entryBlock.retVoid()

    assert(function.toIr ===
      "define void @donothing() {\n" +
      "entry:\n" +
      "\tret void\n" +
      "}")
  }

  test("function returning arg def") {
    val result = Result(IntegerType(32), Set())
    
    val namedArguments = List("testArg" -> Argument(IntegerType(32)))
   
    val function = new IrFunctionBuilder(
      result=result,
      name="retArg",
      namedArguments=namedArguments)

    val entryBlock = function.entryBlock
    entryBlock.ret(function.argumentValues("testArg"))

    assert(function.toIr ===
      "define i32 @retArg(i32 %testArg) {\n" +
      "entry:\n" +
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
      "Argument Count" -> IrFunction.Argument(IntegerType(32)),
      "argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8)))))

    val function = new IrFunctionBuilder(
      result=result,
      namedArguments=namedArguments,
      name="main")
      
    val entryBlock = function.entryBlock

    val helloPointer = entryBlock.getelementptr("helloPtr")(
      elementType=IntegerType(8),
      basePointer=helloWorldDef.variable,
      indices=List(0, 0).map(IntegerConstant(IntegerType(32), _))
    )
      
    entryBlock.callDecl(None)(putsDecl, helloPointer :: Nil)
    entryBlock.ret(IntegerConstant(IntegerType(32), 0))

    assert(function.toIr === 
      """|define i32 @main(i32 %"Argument Count", i8** %argv) {
         |entry:
         |	%helloPtr1 = getelementptr [14 x i8]* @helloWorldString, i32 0, i32 0
         |	call i32 @puts(i8* %helloPtr1) nounwind
         |	ret i32 0
         |}""".stripMargin)
  }
  
  test("multi block function def") {
    val result = IrFunction.Result(VoidType, Set())
   
    val function = new IrFunctionBuilder(
      result=result,
      name="donothing",
      namedArguments=Nil)
    
    val entryBlock = function.entryBlock
    val continueBlock = entryBlock.startChildBlock("continue")

    entryBlock. uncondBranch(continueBlock)

    continueBlock.retVoid()

    assert(function.toIr ===
      "define void @donothing() {\n" +
      "entry:\n" +
      "\tbr label %continue1\n" +
      "continue1:\n" +
      "\tret void\n" +
      "}")
  }

}
