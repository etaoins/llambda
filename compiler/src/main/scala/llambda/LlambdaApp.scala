package llambda

import java.io.File

case class Config(
  inputFile : Option[File] = None,
  outputFile : Option[File] = None,
  emitLlvm : Boolean = false,
  optimizeLevel : Integer = 0)

object LlambdaApp extends App {
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
    }text("set optimization level")
  }

  parser.parse(args, Config()) map { config =>
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

        Compiler(input, output,
          optimizeLevel=config.optimizeLevel,
          emitLlvm=config.emitLlvm)

      case None =>
        // Launch the REPL
        Repl()
    }
  }  getOrElse {
    sys.exit(1)
  }

}
