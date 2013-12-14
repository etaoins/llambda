package io.llambda.typegen

import scala.concurrent._
import ExecutionContext.Implicits.global

import java.io._

object GenerateTypes {
  private val writers = List(
    writer.WritePredicates.apply,
    writer.WriteSizeCheck.apply
  ) : List[writer.OutputWriter]

  def apply(celldefFile : File, outputDir : File) {
    // Load and parse the file
    val definitions = DefinitionParser.parseFile(celldefFile)

    // Check for basic sanity
    CheckTopLevelNamespace(definitions)

    // Extract our field tpyes
    val fieldTypes = ProcessFieldTypes(definitions)

    // Extract our cell classes
    val processedTypes = ProcessCellClasses(fieldTypes)(definitions)
    val cellClasses = processedTypes.cellClasses
    
    // Make sure there are no childless abstract cells
    CheckChildlessAbstractCellClasses(cellClasses.values.toList)

    // Run our writers
    // Make sure they all succeed before we start writing
    val writerResults = writers.flatMap(_(processedTypes)).toMap

    writerResults map { case(outputPath, content) =>
      val outputFile = new File(outputDir, outputPath)

      // Make sure the full path exists
      outputFile.getParentFile().mkdirs()

      // Whoever designed with Java standard library needs to be murdered
      // with a rusty spoon
      val fileWriter = new BufferedWriter(
        new OutputStreamWriter(
          new FileOutputStream(outputFile),
          "UTF-8"
        )
      )

      fileWriter.write(content)
      fileWriter.close()
    }
  }
}
