package io.llambda.compiler
import io.llambda

import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._
import planner.PlannedProgram

import codegen.LlambdaTopLevelSignature

class ExternalCompilerException extends Exception

/** Top-level support for compiling or running Llambda programs
  *
  * This provides two classes of functionality: compiling programs to File instances or running programs and capturing
  * their result. Compiling or running can each take either a File, Scheme data or expressions as input. This results
  * in a total of 6 entry functions.
  */
object Compiler {
  private lazy val platformClangFlags : Seq[String] =
    RuntimeBuildFiles.clangFlags.split(";").filterNot(_.isEmpty)

  /** Returns a list of compiler flags to link the passed required libraries */
  private def libraryClangFlags(nativeLibraries : Set[NativeLibrary]) : List[String] = {
    val systemFlags = List(RuntimeBuildFiles.llcorePath)

    // Note that order matters here. The core library needs to come after the stdlib libraries to satisfy their
    // symbols
    (nativeLibraries collect {
      case NativeStaticLibrary(baseName) =>
        s"${RuntimeBuildFiles.buildDirectory}lib${baseName}.a"
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

    // llc -O0 miscompiles on ARM32/LLVM 3.6.1 while it works correctly without any optimisation flags. As -O0 is
    // intended primarily to test our own optimisation passes simply removing it is sufficient.
    val llcCmd = List("llc", "-tailcallopt") ++ (if (optimiseLevel > 0) List(optimiseArg) else Nil)

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

  /** Stub compile that write directly to the passed output file */
  private def invokeFileSinkCompiler(irBytes : Array[Byte], output : File) {
    // Write the IR directly to disk
    val outputStream = new FileOutputStream(output)
    outputStream.write(irBytes)
  }

  /** Creates an executable from the passed planned program */
  private def compilePlanToFile(
      plannedProgram : PlannedProgram,
      output : File,
      config : CompileConfig,
      entryFilenameOpt : Option[String] = None
  ) : Unit = {
    // Generate the LLVM IR
    val irString = codegen.GenProgram(
      functions=plannedProgram.functions,
      compileConfig=config,
      entryFilenameOpt=entryFilenameOpt
    )

    val irBytes = irString.getBytes("UTF-8")

    if (!config.emitLlvm) {
      val nativeLibraries = plannedProgram.requiredNativeLibraries
      val result = invokeLlvmCompiler(irBytes, output, config.optimiseLevel, nativeLibraries)

      if (!result) {
        throw new ExternalCompilerException
      }
    }
    else {
      invokeFileSinkCompiler(irBytes, output)
    }
  }

  private def abstractCompile[T](
      planInput : (T, CompileConfig) => PlannedProgram
  )(input : T, output : File, config : CompileConfig, entryFilenameOpt : Option[String]) : Unit = {
    val plannedProgram = planInput(input, config)

    compilePlanToFile(plannedProgram, output, config, entryFilenameOpt)
  }

  private def abstractRun[T](
      planInput : (T, CompileConfig) => PlannedProgram
  )(input : T, config : CompileConfig, extraEnv : List[(String, String)] = Nil) : RunResult = {
    val plannedProgram = planInput(input, config)

    try {
      return interpreter.InterpretProgram(plannedProgram)
    }
    catch {
      case _ : interpreter.UninterpretableException =>
    }

    val outputFile = File.createTempFile("llambda", null, null)
    outputFile.deleteOnExit()

    try {
      compilePlanToFile(plannedProgram, outputFile, config, None)

      // Create our output streams
      var stdout : Option[InputStream] = None
      var stderr : Option[InputStream] = None

      val outputIO = new ProcessIO(
        stdin  => Unit, // Don't care
        stdoutStream => stdout = Some(stdoutStream),
        stderrStream => stderr = Some(stderrStream)
      )

      // Run and capture the output
      val process = Process(
        command=outputFile.getAbsolutePath,
        cwd=None,
        extraEnv=extraEnv : _*
      ).run(outputIO)

      val exitValue = process.exitValue()

      val stdoutString = Source.fromInputStream(stdout.get, "UTF-8").mkString
      val stderrString = Source.fromInputStream(stderr.get, "UTF-8").mkString

      RunResult(
        stdout=stdoutString,
        stderr=stderrString,
        exitValue=exitValue,
        runMethod=RunResult.ExternalProgram
      )
    }
    finally {
      outputFile.delete()
    }
  }

  /** Plans the specified input file */
  def planFile(input : File, config : CompileConfig) : PlannedProgram =
    planData(SchemeParser.parseFileAsData(input), config)

  /** Plans the specified input Scheme data */
  def planData(data : List[ast.Datum], config : CompileConfig) : PlannedProgram = {
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

    val exprs = frontend.ExtractProgram(data)(loader, frontendConfig)

    planExprs(exprs, config)
  }

  /** Plans the specified input Scheme expressions */
  def planExprs(exprs : List[et.Expr], config : CompileConfig) : PlannedProgram = {
    // Analyse and drop unused top-level defines
    val analysis = analyser.AnalyseExprs(exprs)

    // Plan execution
    val planConfig = planner.PlanConfig(
      schemeDialect=config.schemeDialect,
      optimise=config.optimiseLevel > 1,
      analysis=analysis
    )

    val plannedProgram = planner.PlanProgram(analysis.usedTopLevelExprs)(planConfig)

    if (config.dumpPlan) {
      println(planner.PrettyPrintPlan(plannedProgram.functions))
    }

    plannedProgram
  }

  /** Compiles the specified input file to the specified output binary file
    *
    * @param  input   Input file
    * @param  ouput   Output file. This will be created if it does not exist
    * @param  config  Compile configuration
    */
  def compileFile(input : File, output : File, config : CompileConfig) : Unit =
    abstractCompile(planFile)(input, output, config, Some(input.getPath))

  /** Compiles the specified input Scheme data to the specified output binary file */
  val compileData =
    abstractCompile(planData)_

  /** Compiles the specified input expressions to the specified output binary file */
  val compileExprs =
    abstractCompile(planExprs)_

  /** Runs the specified input file and returns the result */
  def runFile =
    abstractRun(planFile)_

  /** Runs the specified input Scheme data and returns the result */
  val runData =
    abstractRun(planData)_

  /** Runs the specified input expressions and returns the result */
  val runExprs =
    abstractRun(planExprs)_
}

