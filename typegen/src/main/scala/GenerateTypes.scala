package io.llambda.typegen

import java.io.File

object GenerateTypes {
  def apply(celldefFile : File, outputDir : File) {
    // Load and parse the file
    val definitions = DefinitionParser.parseFile(celldefFile)

    // Check for basic sanity
    CheckTopLevelNamespace(definitions)

    // Extract our field tpyes
    val fieldTypes = ProcessFieldTypes(definitions)

    // Extract our cell classes
    val processedCellClasses = ProcessCellClasses(fieldTypes)(definitions)
    val cellClasses = processedCellClasses.cellClasses
    
    // Make sure there are no childless abstract cells
    CheckChildlessAbstractCellClasses(cellClasses.values.toList)
  }
}
