package io.llambda.typegen

import collection.immutable.ListMap

import io.llambda.llvmir

object ProcessFieldTypes {
  def apply(definitions: List[ParsedDefinition]): ListMap[String, FieldType] = {
    val predefinedFieldTypes = ListMap[String, FieldType](PredefinedFieldTypes().toSeq: _*)

    // Create field types for our cells and aliases
    definitions.foldLeft(predefinedFieldTypes) { case (fieldTypes, definition) =>
      definition match {
        case cellDeclLike: ParsedCellClassDeclarationLike =>
          // This is a cell type
          val cellNames = CellClassNames(cellDeclLike.name)

          fieldTypes + (cellDeclLike.name ->
            PrimitiveFieldType(
              signed=None,
              llvmType=llvmir.UserDefinedType(cellNames.llvmName),
              cppTypeName=cellNames.cppClassName
            )
          )

        case userFieldType: ParsedFieldTypeAlias =>
          val resolvedAlias = ResolveParsedType(fieldTypes)(userFieldType.aliasedType)

          // Default to the definition name if the C name isn't given
          val cppTypeName = userFieldType.cppType.map(_.name)

          // If no cppname is specified then this is purely a front-end alias
          val needsDefinition = userFieldType.cppType.map(_.needsDefinition).getOrElse(false)

          val fieldTypeAlias = new FieldTypeAlias(
              resolvedAlias,
              cppTypeName=cppTypeName,
              needsDefinition=needsDefinition
          )

          val positionedFieldTypeAlias = fieldTypeAlias.setPos(userFieldType.pos)

          fieldTypes + (userFieldType.name -> positionedFieldTypeAlias)
      }
    }
  }
}
