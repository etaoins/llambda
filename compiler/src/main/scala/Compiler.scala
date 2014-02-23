package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private val conniverPasses = List(
    conniver.DisposeValues
  )

  abstract class RunningCompiler {
    def sendLlvmIr(irBytes : Array[Byte]) : Boolean
    def finish() : Unit
  }

  private lazy val platformClangFlags : List[String] =
    if (scala.util.Properties.isMac) {
      // Force use of libc++ on Mac OS X
      // This matches the logic of runtime/CMakeLists.txt
      List("-stdlib=libc++")
    }
    else {
      scala.util.Properties.osName match {
        case "Linux" =>   List("-lpthread")
        case "FreeBSD" => List("-pthread")
        case _ =>         Nil
      }
    }

  private def startLlvmCompiler(output : File, optimizeLevel : Int) : RunningCompiler = {
    val inputStream = new PipedInputStream
    val outputStream = new PipedOutputStream(inputStream)

    val optimizeArg = s"-O${optimizeLevel}"


    val llcCmd = List("llc", optimizeArg)
    val clangCmd = List("clang++", optimizeArg) ++
      platformClangFlags ++
      List("-x", "assembler", "-") ++ 
      List("-x", "none", "runtime/liblliby.a") ++
      List("-o", output.getAbsolutePath)

    val compilePipeline = if (optimizeLevel > 1) { 
      val optCmd = List("opt", optimizeArg)

      optCmd #< inputStream #| llcCmd #| clangCmd
    }
    else {
      llcCmd #< inputStream #| clangCmd
    }

    // Run the compiler pipeline in the background
    val runningProcess = compilePipeline.run

    // Return an object to control the compiler with
    new RunningCompiler {
      def sendLlvmIr(irBytes : Array[Byte]) : Boolean = {
        outputStream.write(irBytes)
        outputStream.close()
        runningProcess.exitValue() == 0 
      }

      def finish() {
        outputStream.close()
      }
    }
  }

  def startFileSinkCompiler(output : File) : RunningCompiler = 
    new RunningCompiler {
      def sendLlvmIr(irBytes : Array[Byte]) : Boolean  = {
        // Write the IR directly to disk
        val outputStream = new FileOutputStream(output)
        outputStream.write(irBytes)
        
        true
      }

      def finish() {
        // Nothing to do
      }
    }
  
  def compileFile(input : File, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseFileAsData(input), output, config)
  
  def compileString(inputString : String, output : File, config : CompileConfig) : Unit =
    compileData(SchemeParser.parseStringAsData(inputString), output, config)

  def compileData(data : List[ast.Datum], output : File, config : CompileConfig) : Unit = {
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
    
    // We're reasonably sure the input program is sane at this point
    // Start our backend compiler and get a control object
    val runningCompiler = if (!config.emitLlvm) {
      startLlvmCompiler(output, config.optimizeLevel)
    }
    else {
      startFileSinkCompiler(output)
    }

    try {
      val optimizedFunctions = if (config.optimizeLevel > 1) {
        conniverPasses.foldLeft(functions) { case (functions, conniverPass) =>
          conniverPass(functions)
        }
      }
      else {
        functions
      }

      // Plan our cell allocations after all optimizatins have been done
      val allocatedFunctions = optimizedFunctions.mapValues(planner.PlanCellAllocations(_))

      // Generate the LLVM IR
      val llvmIr = codegen.GenProgram(allocatedFunctions, config.targetPlatform, featureIdentifiers)
      
      // Send the IR to our running compiler
      if (!runningCompiler.sendLlvmIr(llvmIr.getBytes("UTF-8"))) {
        throw new ExternalCompilerException
      }
    }
    finally {
      runningCompiler.finish()
    }
  }
}

