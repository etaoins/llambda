package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class ExternalCompilerException extends Exception

object Compiler {
  private val conniverPasses = List[conniver.Conniver](
    conniver.MergeIdenticalSteps,
    conniver.UnboxEarly,
    conniver.FindTailCalls
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

  /** Returns a list of compiler flags to link the passed required libraries */
  private def libraryClangFlags(nativeLibraries : Set[NativeLibrary]) : List[String] = {
    val systemFlags = List("build/libllcore.a")

    // Note that order matters here. The core library needs to come after the stdlib libraries to satisfy their
    // symbols
    (nativeLibraries collect {
      case NativeStaticLibrary(baseName) =>
        s"build/lib${baseName}.a"
    }).toList ++ systemFlags
  }

  /** Invokes the LLVM compiler pipeline without creating intermediate files
    *
    * This is the most efficient way to invoke LLVM if intermediates aren't required
    */
  private def invokeLlvmCompiler(
      irBytes : Array[Byte],
      output : File,
      optimiseLevel : Int,
      nativeLibraries : Set[NativeLibrary]
  ) : Boolean = {
    val optimiseArg = s"-O${optimiseLevel}"

    val llcCmd = List("llc", "-tailcallopt", optimiseArg)
    val clangCmd = List("clang++", optimiseArg) ++
      platformClangFlags ++
      List("-x", "assembler", "-") ++ 
      List("-x", "none") ++
      libraryClangFlags(nativeLibraries) ++
      List("-o", output.getAbsolutePath)

    val compilePipeline = if (optimiseLevel > 1) {
      val optCmd = List("opt", "-tailcallopt", optimiseArg)

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
    val (irString, nativeLibraries) = compileDataToIr(
      data=data,
      config=config,
      entryFilenameOpt=entryFilenameOpt
    )

    val irBytes = irString.getBytes("UTF-8")

    if (!config.emitLlvm) {
      val result = invokeLlvmCompiler(irBytes, output, config.optimiseLevel, nativeLibraries)

      if (!result) {
        throw new ExternalCompilerException
      }
    }
    else {
      invokeFileSinkCompiler(irBytes, output)
    }
  }

  def compileDataToIr(
      data : List[ast.Datum],
      config : CompileConfig,
      entryFilenameOpt : Option[String] = None
  ) : (String, Set[NativeLibrary]) = {
    // Prepare to extract
    val loader = new frontend.LibraryLoader(config.targetPlatform)
    val featureIdentifiers = FeatureIdentifiers(config.targetPlatform, config.schemeDialect, config.extraFeatureIdents) 

    // Extract expressions
    val frontendConfig = frontend.FrontendConfig(
      includePath=config.includePath,
      featureIdentifiers=featureIdentifiers,
      schemeDialect=config.schemeDialect,
      traceMacroExpansion=config.traceMacroExpansion
    )

    val exprs = frontend.ExtractProgram(entryFilenameOpt, data)(loader, frontendConfig)

    // Analyse and drop unused top-level defines
    val analysis = analyser.AnalyseExprs(exprs)
    
    // Plan execution
    val planConfig = planner.PlanConfig(
      schemeDialect=config.schemeDialect,
      optimise=config.optimiseLevel > 1,
      analysis=analysis
    )

    val plannedProgram = planner.PlanProgram(analysis.usedTopLevelExprs)(planConfig)

    val nativeLibraries = plannedProgram.requiredNativeLibraries
    val functions = plannedProgram.functions

    val optimisedFunctions = if (config.optimiseLevel > 1) {
      conniverPasses.foldLeft(functions) { case (functions, conniverPass) =>
        conniverPass(functions)
      }
    }
    else {
      // This is required for correctness
      conniver.FindTailCalls(functions)
    }

    // Dispose any unused values
    val disposedFunctions = optimisedFunctions.mapValues(planner.DisposeValues(_))

    // Plan our cell allocations after all optimisations have been done
    val allocatedFunctions = disposedFunctions.mapValues(planner.PlanCellAllocations(_))

    if (config.dumpPlan) {
      println(planner.PrettyPrintPlan(allocatedFunctions))
    }

    // Generate the LLVM IR
    val irString = codegen.GenProgram(
      functions=allocatedFunctions,
      compileConfig=config,
      entryFilenameOpt=entryFilenameOpt
    )

    (irString, nativeLibraries)
  }
}

