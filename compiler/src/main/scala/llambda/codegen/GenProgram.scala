package llambda.codegen

import llambda._
import llambda.codegen.llvmir._
import scala.io.Source

object GenProgram {
  private val llibyInitDecl = {
    IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="lliby_init",
      arguments=Nil,
      attributes=Set(IrFunction.NoUnwind)
    )
  }

  def resourceAsString(resourcePath : String) : String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    io.Source.fromInputStream(stream).mkString
  }

  def preludeIr : String = {
    List(
      resourceAsString("generated/boxedTypes.ll"),
      resourceAsString("defines.ll")
    ) mkString "\n"
  }

  def apply(expressions : List[et.Expression]) : String = {
    val module = new llvmir.IrModuleBuilder {
      // Define main()
      val result = IrFunction.Result(IntegerType(32))
      val namedArguments = List(
        "argc" -> IrFunction.Argument(IntegerType(32)),
        "argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8))))
      )

      declareFunction(llibyInitDecl)

      val mainFunction = new IrFunctionBuilder(
        result=result,
        namedArguments=namedArguments,
        name="main") 

      val entryBlock = mainFunction.entryBlock

      // Initialize our runtime
      entryBlock.callDecl(None)(llibyInitDecl, Nil)
      entryBlock.ret(IntegerConstant(IntegerType(32), 0))

      defineFunction(mainFunction)
    }

    preludeIr + "\n" + module.toIr + "\n"
  }
}

