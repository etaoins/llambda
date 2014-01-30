package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private val conniverPasses = List(
    conniver.DisposeValues,
    conniver.MergeCellAllocations
  )

  private def startLlvmCompiler(llvmIrStream : InputStream, output : File, optimizeLevel : Int) : () => Int = {
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
      List("-x", "none", "runtime/liblliby.a") ++
      List("-o", output.getAbsolutePath)

    val compilePipeline = if (optimizeLevel > 1) { 
      val optCmd = List("opt", optimizeArg)

      optCmd #< llvmIrStream #| llcCmd #| clangCmd
    }
    else {
      llcCmd #< llvmIrStream #| clangCmd
    }

    // Run the compiler pipeline in the background
    val runningProcess = compilePipeline.run

    // Return a function that will wait for the result of the compiler
    () => {
      runningProcess.exitValue()
    }
  }
  
  def compileFile(input : File, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseFileAsData(input), output, config)
  
  def compileString(inputString : String, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseStringAsData(inputString), output, config)

  def compileData(data : List[ast.Datum], output : File, config : CompileConfig) : Unit = {
    // Create our output stream
    val (outputStream, waitFunction) = if (config.emitLlvm) {
      // Write the IR directly to disk
      (new FileOutputStream(output), () => 0)
    }
    else {
      val inputStream = new PipedInputStream
      val outputStream = new PipedOutputStream(inputStream)

      // Start the compiler in the background (threading!)
      val waiter = startLlvmCompiler(inputStream, output, config.optimizeLevel)

      (outputStream, waiter) 
    }

    // Prepare to extract
    val loader = new frontend.LibraryLoader(config.targetPlatform)
    val featureIdentifiers = FeatureIdentifiers(config.targetPlatform, config.extraFeatureIdents) 

    // Extract expressions
    val frontendConfig = frontend.FrontendConfig(
      includePath=config.includePath,
      featureIdentifiers=featureIdentifiers 
    )

    val expressions = frontend.ExtractProgram(data)(loader, frontendConfig)

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

    val optimizedFunctions = if (config.optimizeLevel > 1) {
      conniverPasses.foldLeft(functions) { case (functions, conniverPass) =>
        conniverPass(functions)
      }
    }
    else {
      functions
    }

    // Generate the LLVM IR
    val llvmIr = codegen.GenProgram(optimizedFunctions, config.targetPlatform, featureIdentifiers)
    
    // Write to the output stream
    outputStream.write(llvmIr.getBytes("UTF-8"))
    outputStream.close()

    // Wait for the compiler to finish
    if (waitFunction() != 0) {
      throw new ExternalCompilerException
    }
  }
}

