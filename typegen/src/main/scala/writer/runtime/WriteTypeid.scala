package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

object WriteTypeid extends writer.OutputWriter {
  def apply(processedTypes: ProcessedTypes): Map[String, String] = {
    // Get information about our type tag
    val typeTagField = processedTypes.rootCellClass.typeTagField
    val typeTagFieldType = typeTagField.fieldType

    val typeTagAlias = typeTagFieldType match {
      case alias: FieldTypeAlias =>
        alias

      case _ =>
        // CheckTypeTagField should have caught this
        throw new Exception
    }

    val typeTagCppName = typeTagAlias.cppTypeName.get
    val typeTagSupertype = FieldTypeToCpp(typeTagAlias.aliasedType, None)

    val incBuilder = new CppIncludeBuilder("_LLIBY_BINDING_TYPEID_H")

    incBuilder.appendRaw(writer.GeneratedClikeFileComment)

    // Our C++ type names assume this is included
    incBuilder += "#include <cstdint>"
    incBuilder.sep()

    incBuilder += "namespace lliby"
    incBuilder += "{"
    incBuilder.sep()

    incBuilder += s"enum class ${typeTagCppName} : ${typeTagSupertype}"
    incBuilder += "{"
    incBuilder.indented {
      incBuilder += "Invalid = 0,"
      for(cellClass <- processedTypes.cellClasses.values; typeId <- cellClass.typeId) {
        incBuilder += s"${cellClass.name} = ${typeId},"
      }
    }
    incBuilder += "};"

    incBuilder.sep()
    incBuilder += "}"
    incBuilder.sep()

    Map("runtime/binding/generated/CellTypeId.h" -> incBuilder.toString)
  }
}

