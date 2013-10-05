package llambda

import java.io.{File,FileOutputStream,ByteArrayInputStream}
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private def compileLlvmIr(llvmIr : String, output : File, optimizeLevel : Int) {
    val optimizeArg = s"-O${optimizeLevel}"

    val stdlibArg = if (scala.util.Properties.isMac) {
      // Force use of libc++ on Mac OS X
      // This matches the logic of runtime/CMakeLists.txt
      List("-stdlib=libc++")
    }
    else {
      Nil
    }

    val llcCmd = List("llc", optimizeArg)
    val clangCmd = List("clang++", optimizeArg, "../runtime/liblliby.a") ++
      List("-x", "assembler") ++ 
      stdlibArg ++
      List("-", "-o", output.getAbsolutePath)

    val llvmIrStream = new ByteArrayInputStream(llvmIr.getBytes("UTF-8"))
    val compilePipeline = llcCmd #< llvmIrStream #| clangCmd

    if (compilePipeline.! != 0) {
      throw new ExternalCompilerException
    }
  }

  def apply(input : File, output : File, optimizeLevel : Int = 0, emitLlvm : Boolean = false) {
    // Generate the LLVM IR
    val inputString = Source.fromFile(input).mkString
    val data = SchemeParser.parseStringAsData(inputString)

    val loader = new frontend.DefaultLibraryLoader
    val expressions = frontend.ExtractProgram(data)(loader.load)

    val llvmIr = codegen.GenProgram(expressions)

    if (emitLlvm) {
      // Write the IR directly to disk
      val llvmIrOutStream = new FileOutputStream(output)
      llvmIrOutStream.write(llvmIr.getBytes("UTF-8"))
    }
    else {
      // Compile the IR using llc and clang++
      compileLlvmIr(llvmIr, output, optimizeLevel)
    }
  }
}

