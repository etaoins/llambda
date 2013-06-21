package llambda

import java.io.{File,FileOutputStream}
import scala.io.Source
import scala.language.postfixOps

object Compiler {
  def apply(input : File, output : File) {
    val inputString = Source.fromFile(input).mkString
    val data = SchemeParser.parseStringAsData(inputString)

    val loader = new frontend.DefaultLibraryLoader
    val expressions = frontend.ExtractProgram(data)(loader.load)

    val llvmIr = codegen.GenerateCode(expressions)

    val llvmIrOutStream = new FileOutputStream(output)
    llvmIrOutStream.write(llvmIr.getBytes("UTF-8"))
  }
}

