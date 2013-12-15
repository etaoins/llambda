package io.llambda.typegen

import io.llambda.llvmir

object FieldTypeToLlvm {
  def apply(fieldType : FieldType) : llvmir.FirstClassType = {
    fieldType match {
      case PointerFieldType(pointeeType) =>
        llvmir.PointerType(apply(pointeeType))

      case FunctionPointerFieldType(returnType, arguments) =>
        // Convert our inner types recursively
        val returnTypeLlvm = returnType.map(apply).getOrElse(llvmir.VoidType)
        val argumentsLlvm = arguments.map(apply)

        llvmir.PointerType(
          llvmir.FunctionType(
            returnTypeLlvm,
            argumentsLlvm
          )
        )

      case PrimitiveFieldType(_, llvmType, _) =>
        llvmType

      case alias : FieldTypeAlias =>
        apply(alias.aliasedType)
    }
  }
}

