package io.llambda.typegen

import io.llambda.llvmir

/** Converts a [[FieldType]] to an LLVM type */
object FieldTypeToLlvm {
  def apply(fieldType: FieldType): llvmir.FirstClassType = {
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

      case ArrayFieldType(dimensions, elementType) =>
        val elementTypeLlvm = apply(elementType)

        dimensions.foldRight(elementTypeLlvm) { (elements, innerType) =>
          llvmir.ArrayType(elements, innerType)
        }

      case PrimitiveFieldType(_, llvmType, _) =>
        llvmType

      case alias: FieldTypeAlias =>
        apply(alias.aliasedType)
    }
  }
}

