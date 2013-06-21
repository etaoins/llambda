package llambda

import java.io.File

case class Config(inputFile : Option[File] = None, outputFile : Option[File] = None)

object LlambdaApp extends App {
  val parser = new scopt.OptionParser[Config]("llambda") {
    head("llambda")

    arg[File]("input") optional() action { (file, c) =>
      c.copy(inputFile=Some(file))
    } text("input scheme file")

    opt[File]('o', "out") action { (file, c) =>
      c.copy(outputFile=Some(file))
    } text("output executable filename")
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
          val inputFilePath = input.getAbsolutePath

          if (!inputFilePath.endsWith(".scm")) {
            System.err.println(s"Unable to automatically determine output filename")
            sys.exit(1)
          }

          new File(".scm$".r.replaceAllIn(inputFilePath, ".ll"))
        }

        Compiler(input, output)

      case None =>
        // Launch the REPL
        Repl()
    }
  }  getOrElse {
    sys.exit(1)
  }

}
