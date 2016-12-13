package io.llambda.typegen

/** Checks the sanity of the top-level type namespace
  *
  * If any declared classes are not eventually defined then
  * [[UndefinedCellClassException]] in thrown. If a duplicate type names are
  * encountered then [[DuplicateTypeNameException]] is thrown.
  */
object CheckTopLevelNamespace {
  def apply(definitions: List[ParsedDefinition]) {
    val predefinedTypeNames = PredefinedFieldTypes().keys.toSet

    // Split our definitions up
    val parsedCellDecls = definitions collect {
      case cellDecl: ParsedCellClassDeclaration =>
        cellDecl
    }

    val parsedCellDefs = definitions collect {
      case cellDef: ParsedCellClassDefinition =>
        cellDef
    }

    val parsedUserFieldTypes = definitions collect {
      case userFieldType: ParsedFieldTypeAlias =>
        userFieldType
    }

    // Make sure all declared cell classes are eventually defined
    for(cellDecl <- parsedCellDecls) {
      if (!parsedCellDefs.exists(_.name == cellDecl.name)) {
        throw new UndefinedCellClassException(cellDecl, cellDecl.name)
      }
    }

    // Make sure all definition names are unique and collect their names
    (parsedCellDefs ++ parsedUserFieldTypes).foldLeft(predefinedTypeNames) {
      case (seenNames, definition) =>
        if (seenNames.contains(definition.name)) {
          throw new DuplicateTypeNameException(definition)
        }

        seenNames + definition.name
    }
  }
}

