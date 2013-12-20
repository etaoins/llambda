package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

object WriteSizeCheck extends writer.OutputWriter {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    // Get a list of non-abstract cell classes
    val nonAbstractCellClasses = processedTypes.cellClasses.values.filter(_.instanceType != CellClass.Abstract)
    
    // Start our builder
    val cppBuilder = new CppBuilder

    cppBuilder.appendRaw(writer.GeneratedClikeFileComment)

    nonAbstractCellClasses foreach { cellClass =>
      cppBuilder += "#include \"binding/" + cellClass.names.cppClassName + ".h\""
    }

    cppBuilder.sep()

    // static_assert(sizeof(lliby::RecordCell) <= sizeof(lliby::alloc::Cell), "RecordCell does not fit in to a cell");
    for(cellClass <- nonAbstractCellClasses) {
      val cppName = cellClass.names.cppClassName
      val assertString = "\"" + s"${cppName} does not fit in to a cell" + "\""

      cppBuilder += s"static_assert(sizeof(lliby::${cppName}) <= sizeof(lliby::alloc::Cell), ${assertString});"
    }

    Map("runtime/alloc/generated/sizecheck.h" -> cppBuilder.toString)
  }
}

