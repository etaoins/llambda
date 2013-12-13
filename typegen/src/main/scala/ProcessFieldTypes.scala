package io.llambda.typegen

import collection.immutable.ListMap

import io.llambda.llvmir

object ProcessFieldTypes {
  private case class ResolvedType(llvmType : llvmir.FirstClassType, signed : Option[Boolean])

  private def resolveParsedType(parsedType : ParsedType)(implicit knownFieldTypes : Map[String, FieldType]) : ResolvedType =
    parsedType match {
      case ParsedPointerType(pointeeType) =>
        ResolvedType(llvmir.PointerType(resolveParsedType(pointeeType).llvmType), None)

      case ParsedFunctionPointerType(returnType, arguments) =>
        // We use an Option to represent the return type while LLVM has a void
        // pseudo-type
        val llvmReturnType = returnType.map(resolveParsedType(_).llvmType).getOrElse(llvmir.VoidType)

        val llvmType = llvmir.PointerType(
          llvmir.FunctionType(llvmReturnType, arguments.map(resolveParsedType(_).llvmType))
        )

        ResolvedType(llvmType, None)

      case parsedTypeName @ ParsedTypeName(name) =>
        // Look up the name from our already defined types
        val fieldType = knownFieldTypes.getOrElse(name, {
          throw new UnknownTypeException(parsedTypeName)
        })

        ResolvedType(fieldType.llvmType, fieldType.signed)
    }

  def apply(definitions : List[ParsedDefinition]) : ListMap[String, FieldType] = {
    val predefinedFieldTypes = ListMap(PredefinedFieldTypes().toSeq : _*)
    
    // Create all of our field types
    definitions.foldLeft(predefinedFieldTypes) { case (fieldTypes, definition) =>
      definition match {
        case cellDeclLike : ParsedCellClassDeclarationLike =>
          // This is a cell type
          val cellNames = CellClassNames(cellDeclLike.name)

          fieldTypes + (cellDeclLike.name ->
            FieldType(
              signed=None,
              llvmType=llvmir.UserDefinedType(cellNames.llvmName),
              cppTypeName=cellNames.cppName,
              // This is declared elsewere
              needsDefinition=false
            )
          )

        case userFieldType : ParsedUserDefinedFieldType =>
          val superType = resolveParsedType(userFieldType.aliasOf)(fieldTypes)

          // Default to the definition name if the C name isn't given
          val cppTypeName = userFieldType.cppType.map(_.name).getOrElse(userFieldType.name)

          // Default to needing definition unless otherwise specified
          val needsDefinition = userFieldType.cppType.map(_.needsDefinition).getOrElse(true)

          fieldTypes + (userFieldType.name ->
            FieldType(
              signed=superType.signed,
              llvmType=superType.llvmType,
              cppTypeName=cppTypeName,
              needsDefinition=needsDefinition
            )
          )
      }
    }
  }
}
