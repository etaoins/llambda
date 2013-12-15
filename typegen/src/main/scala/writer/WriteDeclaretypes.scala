package io.llambda.typegen.writer

import io.llambda.typegen._

object WriteDeclaretypes {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    val incBuilder = new CppIncludeBuilder("_LLIBY_BINDING_DECLARETYPES_H")

    incBuilder.appendRaw(GeneratedFileComment)

    incBuilder += "namespace lliby"
    incBuilder += "{"
    incBuilder.sep()

    // Forward declare each cell class
    for(cellClass <- processedTypes.cellClasses.values) {
      val cppName = cellClass.names.cppName
      incBuilder += s"class ${cppName};"
    }
    
    incBuilder.sep()

    // Add all of the typedefs
    val typeTagFieldType = processedTypes.rootCellClass.typeTagField.fieldType

    for(fieldType <- processedTypes.fieldTypes.values) {
      fieldType match {
        case alias : FieldTypeAlias if (alias != typeTagFieldType) && alias.needsDefinition =>
          val cppTypeName = alias.cppTypeName.get
          incBuilder += "typedef " + FieldTypeToCpp(alias.aliasedType, Some(cppTypeName)) + ";"

        case _ =>
          // Must already be defined elsewhere in C++
      }
    }

    incBuilder.sep()
    incBuilder += "}"

    Map("runtime/binding/generated/declaretypes.h" -> incBuilder.toString)
  }
}
