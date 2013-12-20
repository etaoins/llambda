package io.llambda.typegen

/** Convert a [[FieldType]] instance to a C++ type name 
  *
  * @param fieldType    [[FieldType]] instance to convert 
  * @param definedName  If None then just the type name is returned. If defined
  *                     then a definition suitable for a member variable or 
  *                     typedef is generated
  */
object FieldTypeToCpp {
  def apply(fieldType : FieldType, definedName : Option[String]) : String = {
    // Except for function pointers we can define an instance or name for a type
    // by placing its identifier after the type specifier
    val definedNameSuffix = definedName.map(" " + _).getOrElse("") 

    fieldType match {
      case PointerFieldType(pointeeType) =>
        apply(pointeeType, None) + "*" + definedNameSuffix

      case FunctionPointerFieldType(returnType, arguments) =>
        // Convert our inner types recursively
        val argumentsCpp = arguments.map(apply(_, None)).mkString(", ")
        val returnTypeCpp = returnType.map(apply(_, None)).getOrElse("void")
        val pointerName = definedName.getOrElse("")

        s"${returnTypeCpp} (*${pointerName})(${argumentsCpp})"

      case PrimitiveFieldType(_, _, cppTypeName) =>
        // This is easy
        cppTypeName + definedNameSuffix

      case alias : FieldTypeAlias =>
        alias.cppTypeName match {
          case Some(cppTypeName) =>
            cppTypeName + definedNameSuffix

          case None =>
            // This is a frontend-only alias
            // Just remove the indirection
            apply(alias.aliasedType, definedName)
        }
    }
  }
}
