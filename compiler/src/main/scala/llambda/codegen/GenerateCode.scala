package llambda.codegen

import llambda._
import llambda.codegen.llvmir._
import scala.io.Source

object GenerateCode {
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
    val module = new llvmir.IrModule {
      // Define main()
      val result = IrFunction.Result(IntegerType(32))
      val namedArguments = List(
        "argc" -> IrFunction.Argument(IntegerType(32)),
        "argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8))))
      )

      val mainFunction = new IrFunctionDef(
        result=result,
        namedArguments=namedArguments,
        name="main") {

        addBlock("entry")(new IrBlock {
          ret(IntegerConstant(IntegerType(32), 0))
        })
      }

      defineFunction(mainFunction)
    }

    preludeIr + "\n" + module.toIr + "\n"
  }
}

