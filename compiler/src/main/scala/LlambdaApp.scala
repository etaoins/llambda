package io.llambda.compiler
import io.llambda

import java.io.File

object LlambdaApp extends App {
  case class Config(
    inputFile : Option[File] = None,
    outputFile : Option[File] = None,
    emitLlvm : Boolean = false,
    optimizeLevel : Int = 0,
    targetPlatformOpt : Option[platform.TargetPlatform] = None)
  
  private val stringToPlatform = Map(
    ("posix64" -> platform.Posix64),
    ("posix32" -> platform.Posix32),
    ("win32"   -> platform.Win32),
    ("win64"   -> platform.Win64)
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

    opt[Int]('O', "optimize") action { (level, c) =>
      c.copy(optimizeLevel=level)
    } validate { level =>
      if ((level < 0) || (level > 3)) {
        failure("Optimization level must be between 0 and 3 inclusive")
      }
      else {
        success
      }
    } text("set optimization level")

    opt[String]("target-platform") action { (platformString, c) =>
      c.copy(targetPlatformOpt=Some(stringToPlatform(platformString)))
    } validate { platformString =>
      if (!stringToPlatform.contains(platformString)) {
        val validPlatforms = stringToPlatform.keys.toList.sorted
        failure("Unknown target platform. Valid values are: " + validPlatforms.mkString(", "))
      }
      else {
        success
      }
    } text("target platform")

    help("help")
  }

  parser.parse(args, Config()) map { config =>
    // Determine our target platform
    val targetPlatform = config.targetPlatformOpt getOrElse {
      platform.DetectJvmPlatform()
    }

    config.inputFile match {
      case Some(input) =>
        // Make sure this is a real file that exists
        if (!input.canRead) {
          System.err.println(s"Unable to read ${input.getAbsolutePath}")
          sys.exit(1)
        }

        val output = config.outputFile getOrElse {
          // Try to intelligently build our output path
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

        // Get the URL of the file's parent directory
        val inputDirUrl = input.getParentFile.toURI.toURL

        // Build the include path
        val includePath = frontend.IncludePath(
          fileParentDir=Some(inputDirUrl),
          packageRootDir=Some(inputDirUrl)
        )

        // Create our compiler config
        val compileConfig = CompileConfig(
          includePath=includePath,
          emitLlvm=config.emitLlvm,
          optimizeLevel=config.optimizeLevel,
          targetPlatform=targetPlatform)

        Compiler.compileFile(input, output, compileConfig)

      case None =>
        // Launch the REPL
        val repl = new Repl(targetPlatform)
        repl()
    }
  }  getOrElse {
    sys.exit(1)
  }

}
