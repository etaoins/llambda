package io.llambda.compiler
import io.llambda

import java.io.File

object CompilerApp extends App {
  case class Config(
    inputFile : Option[File] = None,
    outputFile : Option[File] = None,
    emitLlvm : Boolean = false,
    optimizeLevel : Int = 0,
    extraFeatureIdents : Set[String] = Set(),
    genDebugInfo : Boolean = false,
    targetPlatformOpt : Option[platform.TargetPlatform] = None,
    schemeDialect : dialect.Dialect = dialect.Dialect.default,
    saveTempObj : Boolean = false,
    dumpPlan : Boolean = false
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
      val validPlatforms = stringToPlatform.keys.toList.sorted
      if (!validPlatforms.contains(platformString)) {
        failure("Unknown target platform. Valid values are: " + validPlatforms.mkString(", "))
      }
      else {
        success
      }
    } text("target platform") 

    opt[String]("scheme-dialect") action { (dialectString, c) =>
      c.copy(schemeDialect=dialect.Dialect.dialects(dialectString))
    } validate { dialectString => 
      val validDialects = dialect.Dialect.dialects.keys.toList.sorted
      if (!validDialects.contains(dialectString)) {
        failure("Unknown Scheme dialect. Valid values are: " + validDialects.mkString(", "))
      }
      else {
        success
      }
    } text("scheme dialect")

    opt[String]("with-feature-ident") unbounded() action { (featureIdent, c) =>
      c.copy(extraFeatureIdents=c.extraFeatureIdents + featureIdent)
    } text("additional feature identifier to provide for (cond-expand)")

    opt[Unit]('g', "generate-debug-info") action { (_, c) =>
      c.copy(genDebugInfo=true)
    } text("generate debugging information")

    opt[Unit]("save-temp-obj") action { (_, c) =>
      c.copy(saveTempObj=true)
    } text ("save intermediate .o file during compilation")

    opt[Unit]("dump-plan") action { (_, c) =>
      c.copy(dumpPlan=true)
    } text ("dump internal execution plan")

    help("help")

    checkConfig { c =>
      if (c.emitLlvm && c.saveTempObj)
        failure("--save-temp-obj does not make sense with --emit-llvm; no objects will be created")
      else
        success
    }
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
          targetPlatform=targetPlatform,
          schemeDialect=config.schemeDialect,
          emitLlvm=config.emitLlvm,
          optimizeLevel=config.optimizeLevel,
          extraFeatureIdents=config.extraFeatureIdents,
          genDebugInfo=config.genDebugInfo,
          saveTempObj=config.saveTempObj,
          dumpPlan=config.dumpPlan
        )

        Compiler.compileFile(input, output, compileConfig)

      case None =>
        // Launch the REPL
        val repl = new Repl(targetPlatform, config.schemeDialect)
        repl()
    }
  }  getOrElse {
    sys.exit(1)
  }

}
