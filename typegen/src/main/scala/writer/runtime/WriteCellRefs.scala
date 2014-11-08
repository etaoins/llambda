package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

object WriteCellRefs extends writer.OutputWriter {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    // Start our builder
    val incBuilder = new CppIncludeBuilder("_LLIBY_ALLOC_CELLREFTYPES_H")

    incBuilder.appendRaw(writer.GeneratedClikeFileComment)
    
    incBuilder += "#include \"binding/" + processedTypes.rootCellClass.names.cppClassName + ".h\""
    incBuilder += "#include \"alloc/StrongRef.h\""
    incBuilder.sep()

    incBuilder += "namespace lliby"
    incBuilder += "{"
    incBuilder += "namespace alloc"
    incBuilder += "{"
    incBuilder.sep()

    for(cellClass <- processedTypes.cellClasses.values) {
      val names = cellClass.names

      incBuilder += s"typedef StrongRef<${names.cppClassName}> ${names.cppRefName};"
    }

    incBuilder.sep()
    
    incBuilder += "}"
    incBuilder += "}"
    incBuilder.sep()

    Map("runtime/alloc/generated/cellreftypes.h" -> incBuilder.toString)
  }
}

