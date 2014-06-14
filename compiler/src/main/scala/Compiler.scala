package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private val conniverPasses = List[conniver.Conniver](
    conniver.MergeIdenticalSteps
  )

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

  private def invokeLlvmCompiler(irBytes : Array[Byte], output : File, optimizeLevel : Int) : Boolean = {
    val optimizeArg = s"-O${optimizeLevel}"

    val llcCmd = List("llc", optimizeArg)
    val clangCmd = List("clang++", optimizeArg) ++
      platformClangFlags ++
      List("-x", "assembler", "-") ++ 
      List("-x", "none", "runtime/liblliby.a") ++
      List("-o", output.getAbsolutePath)

    val compilePipeline = if (optimizeLevel > 1) { 
      val optCmd = List("opt", optimizeArg)

      optCmd #| llcCmd #| clangCmd
    }
    else {
      llcCmd #| clangCmd
    }

    def dumpIrToStdin(stdinStream : OutputStream) {
      stdinStream.write(irBytes)
      stdinStream.close()
    }

    // Run the compiler pipeline in the background
    val runningProcess = compilePipeline.run(new ProcessIO(dumpIrToStdin, _.close, BasicIO.toStdErr))

    runningProcess.exitValue() == 0 
  }

  def invokeFileSinkCompiler(irBytes : Array[Byte], output : File) { 
    // Write the IR directly to disk
    val outputStream = new FileOutputStream(output)
    outputStream.write(irBytes)
  }
  
  def compileFile(input : File, output : File, config : CompileConfig) : Unit =
    compileData(
      data=SchemeParser.parseFileAsData(input),
      output=output,
      config=config,
      entryFilenameOpt=Some(input.getPath)
    )
  
  def compileString(inputString : String, output : File, config : CompileConfig) : Unit =
    compileData(
      data=SchemeParser.parseStringAsData(inputString),
      output=output,
      config=config
    )

  def compileData(data : List[ast.Datum], output : File, config : CompileConfig, entryFilenameOpt : Option[String] = None) : Unit = {
    val irBytes = compileDataToIr(
      data=data,
      config=config,
      entryFilenameOpt=entryFilenameOpt
    ).getBytes("UTF-8")

    if (!config.emitLlvm) {
      if (!invokeLlvmCompiler(irBytes, output, config.optimizeLevel)) {
        throw new ExternalCompilerException
      }
    }
    else {
      invokeFileSinkCompiler(irBytes, output)
    }
  }

  def compileDataToIr(data : List[ast.Datum], config : CompileConfig, entryFilenameOpt : Option[String] = None) : String = {
    // Prepare to extract
    val loader = new frontend.LibraryLoader(config.targetPlatform)
    val featureIdentifiers = FeatureIdentifiers(config.targetPlatform, config.extraFeatureIdents) 

    // Extract expressions
    val frontendConfig = frontend.FrontendConfig(
      includePath=config.includePath,
      featureIdentifiers=featureIdentifiers 
    )

    val exprs = frontend.ExtractProgram(data)(loader, frontendConfig)

    // Analyse and drop unused top-level defines
    val analysis = reducer.AnalyseExprs(exprs)

    // Drop unused top-level bindings
    // Otherwise we produce a lot of unused LLVM IR on -O 0
    val droppedExprs = analysis.usedTopLevelExprs

    val reducedExprs = if (config.optimizeLevel > 1) {
      // Reduce the expressions
      List(reducer.ReduceExprs(analysis))
    }
    else {
      droppedExprs
    }
    
    // Plan execution
    val planConfig = planner.PlanConfig(
      optimize=config.optimizeLevel > 1,
      analysis=analysis
    )

    val functions = planner.PlanProgram(reducedExprs)(planConfig)
    
    val optimizedFunctions = if (config.optimizeLevel > 1) {
      conniverPasses.foldLeft(functions) { case (functions, conniverPass) =>
        conniverPass(functions)
      }
    }
    else {
      functions
    }

    // Dispose any unused values
    val disposedFunctions = optimizedFunctions.mapValues(planner.DisposeValues(_))

    // Plan our cell allocations after all optimizations have been done
    val allocatedFunctions = disposedFunctions.mapValues(planner.PlanCellAllocations(_))

    // Generate the LLVM IR
    codegen.GenProgram(
      functions=allocatedFunctions,
      compileConfig=config,
      featureIdentifiers=featureIdentifiers,
      entryFilenameOpt=entryFilenameOpt
    )
  }
}

