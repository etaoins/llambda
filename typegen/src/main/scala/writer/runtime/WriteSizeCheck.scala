package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

object WriteSizeCheck extends writer.OutputWriter {
  private def writeCellClassSizeCheck(cppBuilder: CppBuilder, cellClass: CellClass) {
    val cppName = cellClass.names.cppClassName
    val assertString = "\"" + s"${cppName} does not fit in to a cell" + "\""

    // static_assert(sizeof(lliby::RecordCell) <= sizeof(lliby::alloc::Cell), "RecordCell does not fit in to a cell");
    cppBuilder += s"static_assert(sizeof(lliby::${cppName}) <= sizeof(AllocCell), ${assertString});"
  }

  def apply(processedTypes: ProcessedTypes): Map[String, String] = {
    // Get a list of non-abstract cell classes
    val nonAbstractCellClasses = processedTypes.cellClasses.values.filter({ cellClass =>
      cellClass.instanceType != CellClass.Abstract
    })

    // Start our builder
    val cppBuilder = new CppBuilder

    cppBuilder.appendRaw(writer.GeneratedClikeFileComment)

    // Assume variants are defined in the same header file as their parent class
    for(cellClass <- nonAbstractCellClasses if cellClass.instanceType != CellClass.Variant) {
      cppBuilder += "#include \"binding/" + cellClass.names.cppClassName + ".h\""
    }

    cppBuilder.sep()
    cppBuilder += "using lliby::alloc::AllocCell;"
    cppBuilder.sep()

    for(cellClass <- nonAbstractCellClasses) {
      writeCellClassSizeCheck(cppBuilder, cellClass)
    }

    Map("runtime/alloc/generated/sizecheck.h" -> cppBuilder.toString)
  }
}

