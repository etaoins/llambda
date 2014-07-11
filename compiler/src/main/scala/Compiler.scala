package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private val runtimeObjPath = "build/liblliby.a"

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

  /** Invokes the LLVM compiler pipeline without creating intermediate files
    *
    * This is the most efficient way to invoke LLVM if intermediates aren't required
    */
  private def invokeDirectLlvmCompiler(irBytes : Array[Byte], output : File, optimizeLevel : Int) : Boolean = {
    val optimizeArg = s"-O${optimizeLevel}"

    val llcCmd = List("llc", optimizeArg)
    val clangCmd = List("clang++", optimizeArg) ++
      platformClangFlags ++
      List("-x", "assembler", "-") ++ 
      List("-x", "none", runtimeObjPath) ++
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
  
  /** Invokes the LLVM compiler pipeline while saving the intermediate object file
    *
    * This requires two compilation stages - use invokeDirectLlvmCompiler where possible
    */
  private def invokeTempSavingLlvmCompiler(irBytes : Array[Byte], output : File, optimizeLevel : Int) : Boolean = {
    val objOutputPath = output.getAbsolutePath + ".o"
    val optimizeArg = s"-O${optimizeLevel}"

    val llcCmd = List("llc", optimizeArg) ++
      List("-filetype=obj") ++
      List("-o", objOutputPath)

    // Build a pipeline for our object file
    val objFilePipeline = (if (optimizeLevel > 1) { 
      val optCmd = List("opt", optimizeArg)
      optCmd #| llcCmd
    }
    else {
      llcCmd
    }) : ProcessBuilder
    
    def dumpIrToStdin(stdinStream : OutputStream) {
      stdinStream.write(irBytes)
      stdinStream.close()
    }

    val runningObjFileProc = objFilePipeline.run(new ProcessIO(dumpIrToStdin, _.close, BasicIO.toStdErr))

    if (runningObjFileProc.exitValue() != 0) {
      // Failed!
      return false
    }
    
    val clangCmd = List("clang++", optimizeArg) ++
      platformClangFlags ++
      List(objOutputPath) ++
      List(runtimeObjPath) ++
      List("-o", output.getAbsolutePath)


    clangCmd.run().exitValue() == 0
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
      val result = if (config.saveTempObj) {
        invokeTempSavingLlvmCompiler(irBytes, output, config.optimizeLevel)
      }
      else {
        invokeDirectLlvmCompiler(irBytes, output, config.optimizeLevel)
      }

      if (!result) {
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

    val exprs = frontend.ExtractProgram(entryFilenameOpt, data)(loader, frontendConfig)

    // Analyse and drop unused top-level defines
    val analysis = analyser.AnalyseExprs(exprs)
    
    // Plan execution
    val planConfig = planner.PlanConfig(
      optimize=config.optimizeLevel > 1,
      analysis=analysis
    )

    val functions = planner.PlanProgram(analysis.usedTopLevelExprs)(planConfig)
    
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

