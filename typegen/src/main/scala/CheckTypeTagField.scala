package io.llambda.typegen

import io.llambda.llvmir

/** Verifies the sanity of the type tag field */
object CheckTypeTagField {
  def apply(rootCellClass: RootCellClass) {
    val typeTagField = rootCellClass.typeTagField
    val fieldType = typeTagField.fieldType

    // The type must be an alias with a C++ type so we can write an enum
    val fieldTypeAlias = fieldType match {
      case alias: FieldTypeAlias =>
        alias

      case _ =>
        throw new NonAliasedTypeTagFieldException(typeTagField)
    }

    if (!fieldTypeAlias.cppTypeName.isDefined) {
      throw new TypeTagAliasMissingCppNameException(fieldTypeAlias)
    }

    if (!fieldTypeAlias.needsDefinition) {
      throw new TypeTagAliasExternallyDefinedException(fieldTypeAlias)
    }

    fieldTypeAlias.aliasedType match {
      case PrimitiveFieldType(Some(_), llvmir.IntegerType(_), _) =>
        // Good to go

      case _ =>
        throw new TypeTagAliasNonIntegralException(fieldTypeAlias)
    }

  }
}
