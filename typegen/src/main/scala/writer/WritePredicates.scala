package io.llambda.typegen.writer

import io.llambda.typegen._

object WritePredicates {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    // Get a list of non-internal cell classes
    val publicCellClasses = processedTypes.cellClasses.values.filter(!_.internal)
    
    // Get the C++ name for the root class
    val rootClassCppName = CellClassNames(processedTypes.rootCellClass.name).cppName

    // Start our builder
    val cppBuilder = new CppBuilder

    cppBuilder.appendRaw(GeneratedFileComment)

    for(cellClass <- publicCellClasses) {
      cppBuilder += "#include \"binding/" + cellClass.names.cppName + ".h\""
    }

    cppBuilder.sep()
    cppBuilder += "using namespace lliby;"
    cppBuilder.sep()
    cppBuilder += "extern \"C\""
    cppBuilder += "{"
    cppBuilder.sep()

    for(cellClass <- publicCellClasses) {
      val functionName = s"lliby_is_${cellClass.names.underscoreName}" 

      cppBuilder += s"bool ${functionName}(const ${rootClassCppName} *value)"

      cppBuilder.blockSep {
        cppBuilder += s"return ${cellClass.names.cppName}::isInstance(value);"
      }
    }
    
    cppBuilder.sep()
    cppBuilder += "}"

    Map("runtime/stdlib/generated/predicates.cpp" -> cppBuilder.toString)
  }
}
