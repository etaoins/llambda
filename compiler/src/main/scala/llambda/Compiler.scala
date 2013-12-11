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
    val clangCmd = List("clang++", optimizeArg) ++
      stdlibArg ++
      List("-x", "assembler", "-") ++ 
      List("-x", "none", "../runtime/liblliby.a") ++
      List("-o", output.getAbsolutePath)

    val llvmIrStream = new ByteArrayInputStream(llvmIr.getBytes("UTF-8"))

    val compilePipeline = if (optimizeLevel > 1) { 
      val optCmd = List("opt", optimizeArg)
      optCmd #< llvmIrStream #| llcCmd #| clangCmd
    }
    else {
      llcCmd #< llvmIrStream #| clangCmd
    }

    if (compilePipeline.! != 0) {
      throw new ExternalCompilerException
    }
  }
  
  def compileFile(input : File, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseFileAsData(input), output, config)
  
  def compileString(inputString : String, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseStringAsData(inputString), output, config)

  def compileData(data : List[ast.Datum], output : File, config : CompileConfig) : Unit = {
    // Parse the program
    val loader = new frontend.LibraryLoader(config.targetPlatform)
    val expressions = frontend.ExtractProgram(data)(loader, config.includePath)

    // Optimize
    val optimizedExpressions = if (config.optimizeLevel > 1) {
      expressions.map(optimize.FlattenSelfExecutingLambdas.apply)
    }
    else {
      expressions
    }

    // Analyize
    val analysis = analyzer.Analyize(optimizedExpressions)

    // Plan execution
    val planConfig = planner.PlanConfig(
      optimize=config.optimizeLevel > 1,
      analysis=analysis
    )

    val functions = planner.PlanProgram(optimizedExpressions)(planConfig)

    // Generate the LLVM IR
    val llvmIr = codegen.GenProgram(functions, config.targetPlatform)

    if (config.emitLlvm) {
      // Write the IR directly to disk
      val llvmIrOutStream = new FileOutputStream(output)
      llvmIrOutStream.write(llvmIr.getBytes("UTF-8"))
    }
    else {
      // Compile the IR using llc and clang++
      compileLlvmIr(llvmIr, output, config.optimizeLevel)
    }
  }
}

