package io.llambda.typegen.writer

import io.llambda.typegen._

object WritePredicates {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    // Get a list of non-internal cell classes
    val publicCellClasses = processedTypes.cellClasses.values.filter(!_.internal)
    
    // Get the C++ name for the root class
    val rootClassCppName = CellClassNames(processedTypes.rootCellClass.name).cppName

    // Get the names for each cell class
    val namedCellClasses = publicCellClasses.map { cellClass =>
      (CellClassNames(cellClass.name), cellClass)
    }

    // Start our builder
    val cppBuilder = new CppBuilder

    cppBuilder.appendRaw(GeneratedFileComment)

    namedCellClasses foreach { case (cellNames, cellClass) =>
      cppBuilder += "#include \"binding/" + cellNames.cppName + ".h\""
    }

    cppBuilder.sep()
    cppBuilder += "using namespace lliby;"
    cppBuilder.sep()
    cppBuilder += "extern \"C\""
    cppBuilder += "{"
    cppBuilder.sep()

    namedCellClasses foreach { case (cellNames, cellClass) =>
      val functionName = s"lliby_is_${cellNames.underscoreName}" 

      cppBuilder += s"bool ${functionName}(const ${rootClassCppName} *value)"

      cppBuilder.block {
        cppBuilder += s"return ${cellNames.cppName}::isInstance(value);"
      }
    }
    
    cppBuilder.sep()
    cppBuilder += "}"
    cppBuilder.sep()

    Map("runtime/stdlib/generated/predicates.cpp" -> cppBuilder.toString)
  }
}
