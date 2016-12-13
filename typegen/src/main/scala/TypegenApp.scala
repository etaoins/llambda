package io.llambda.typegen

import java.io.File

object TypegenApp extends App {
  case class Config(
    celldefFile: File = new File("cellTypes.celldef"),
    outputDir: File = new File(".")
  )

  val parser = new scopt.OptionParser[Config]("typegen") {
    head("typegen")

    arg[File]("celldef file") optional() action { (file, c) =>
      c.copy(celldefFile=file)
    } text("input cell definition file")

    opt[File]('o', "output-dir") action { (file, c) =>
      c.copy(outputDir=file)
    } text("base directory for generated output")

    help("help")
  }

  parser.parse(args, Config()) map { config =>
    GenerateTypes(config.celldefFile, config.outputDir)
  }
}
