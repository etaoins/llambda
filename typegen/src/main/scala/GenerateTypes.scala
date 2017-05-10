package io.llambda.typegen

import java.io._

object GenerateTypes {
  private val writers = List(
    writer.runtime.WriteSizeCheck,
    writer.runtime.WriteDeclaretypes,
    writer.runtime.WriteTypeid,
    writer.runtime.WriteCellMembers,

    writer.compiler.WriteLlvmCellTypes,
    writer.compiler.WriteScalaCellTypes,
    writer.compiler.WriteScalaValueTypes
  ): List[writer.OutputWriter]

  def apply(celldefFile: File, outputDir: File) {
    // Load and parse the file
    val definitions = DefinitionParser.parseFile(celldefFile)

    // Check for basic sanity
    CheckTopLevelNamespace(definitions)

    // Extract our field tpyes
    val fieldTypes = ProcessFieldTypes(definitions)

    // Extract our cell classes
    val processedTypes = ProcessCellClasses(fieldTypes)(definitions)

    // Make sure there are no childless abstract cells
    CheckChildlessAbstractCellClasses(processedTypes)

    // Make sure our type tag field is sane
    CheckTypeTagField(processedTypes.rootCellClass)

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
