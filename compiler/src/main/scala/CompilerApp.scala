package io.llambda.compiler
import io.llambda

import java.io.File
import scala.sys.process._

object CompilerApp extends App {
  case class Config(
    inputFile: Option[File] = None,
    outputFile: Option[File] = None,
    emitLlvm: Boolean = false,
    optimiseLevel: Int = 2,
    extraFeatureIdents: Set[String] = Set(),
    targetPlatformOpt: Option[platform.TargetPlatform] = None,
    dumpPlan: Boolean = false,
    traceMacroExpansion: Boolean = false,
    runAsScript: Boolean = false
  )

  private val stringToPlatform = Map(
    ("posix64be" -> platform.Posix64BE),
    ("posix64le" -> platform.Posix64LE),
    ("posix32be" -> platform.Posix32BE),
    ("posix32le" -> platform.Posix32LE),
    ("win32"     -> platform.Win32),
    ("win64"     -> platform.Win64)
  )

  val parser = new scopt.OptionParser[Config]("llambda") {
    head("llambda")

    arg[File]("input") optional() action { (file, c) =>
      c.copy(inputFile=Some(file))
    } text("input scheme file")

    opt[File]('o', "out") action { (file, c) =>
      c.copy(outputFile=Some(file))
    } text("output executable filename")

    opt[Unit]("emit-llvm") action { (_, c) =>
      c.copy(emitLlvm=true)
    } text("emit LLVM assembler instead of object code")

    opt[Int]('O', "optimise") action { (level, c) =>
      c.copy(optimiseLevel=level)
    } validate { level =>
      if ((level < 0) || (level > 3)) {
        failure("Optimisation level must be between 0 and 3 inclusive")
      }
      else {
        success
      }
    } text("set optimisation level")

    opt[String]("target-platform") action { (platformString, c) =>
      c.copy(targetPlatformOpt=Some(stringToPlatform(platformString)))
    } validate { platformString =>
      val validPlatforms = stringToPlatform.keys.toList.sorted
      if (!validPlatforms.contains(platformString)) {
        failure("Unknown target platform. Valid values are: " + validPlatforms.mkString(", "))
      }
      else {
        success
      }
    } text("target platform")

    opt[String]("with-feature-ident") unbounded() action { (featureIdent, c) =>
      c.copy(extraFeatureIdents=c.extraFeatureIdents + featureIdent)
    } text("additional feature identifier to provide for (cond-expand)")

    opt[Unit]("dump-plan") action { (_, c) =>
      c.copy(dumpPlan=true)
    } text ("dump internal execution plan")

    opt[Unit]("trace-macro-expansion") action { (_, c) =>
      c.copy(traceMacroExpansion=true)
    } text ("trace macro expansion process")

    opt[Unit]('s', "script") action { (_, c) =>
      c.copy(runAsScript=true)
    } text ("run program immediately after compilation and delete the result")

    checkConfig { c =>
      if (c.runAsScript && c.emitLlvm) {
        failure("Cannot run program as script while emitting LLVM assembler")
      }
      else {
        success
      }
    }

    help("help")
  }

  parser.parse(args, Config()) map { config =>
    // Determine our target platform
    val targetPlatform = config.targetPlatformOpt getOrElse {
      platform.DetectTargetPlatform()
    }

    config.inputFile match {
      case Some(input) =>
        // Make sure this is a real file that exists
        if (!input.canRead) {
          System.err.println(s"Unable to read ${input.getAbsolutePath}")
          sys.exit(1)
        }

        val output = config.outputFile getOrElse {
          if (config.runAsScript) {
            // Create a temporary file - we should be able to write to tmp and this won't overwrite any existing
            // binary
            File.createTempFile("llambdascript", null, null)
          }
          else {
            // Try to intelligently build our output path based on the name of our input file
            val inputFilePath = input.getAbsolutePath

            if (!inputFilePath.endsWith(".scm")) {
              System.err.println(s"Unable to automatically determine output filename")
              sys.exit(1)
            }

            val targetExtension = if (config.emitLlvm) {
              ".ll"
            }
            else {
              ""
            }

            new File(".scm$".r.replaceAllIn(inputFilePath, targetExtension))
          }
        }

        if (config.runAsScript) {
          output.deleteOnExit()
        }

        // Get the URL of the file's parent directory
        val inputDirUrl = input.getParentFile.toURI.toURL

        // Build the include path
        val includePath = frontend.IncludePath(List(inputDirUrl))

        // Create our compiler config
        val compileConfig = CompileConfig(
          includePath=includePath,
          targetPlatform=targetPlatform,
          emitLlvm=config.emitLlvm,
          optimiseLevel=config.optimiseLevel,
          extraFeatureIdents=config.extraFeatureIdents,
          dumpPlan=config.dumpPlan,
          traceMacroExpansion=config.traceMacroExpansion
        )

        Compiler.compileFile(input, output, compileConfig)

        if (config.runAsScript) {
          // Proxy our stdin and stdout/stderr to the child process
          val outputIO = new ProcessIO(BasicIO.input(true), BasicIO.toStdOut, BasicIO.toStdErr)

          // Fork a child process
          val testProcess = Process(output.getAbsolutePath).run(outputIO)

          // Delete the temporary executable
          output.delete()

          // Wait for the child process to exit and record the exit value
          val exitValue = testProcess.exitValue()

          // sbt installs a security manager that won't let us call sys.exit and instead throws an exception, prints a
          // message and exits. This makes using sys.exit() for successful termination very noisey.
          if (exitValue != 0) {
            sys.exit(exitValue)
          }
        }

      case None =>
        // Launch the REPL
        (new repl.JlineRepl(targetPlatform))()
    }
  }  getOrElse {
    sys.exit(1)
  }

}
