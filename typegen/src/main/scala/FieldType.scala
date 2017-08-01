package io.llambda.typegen

import scala.util.parsing.input.Positional

import io.llambda.llvmir

/** Type for a cell class field */
sealed abstract class FieldType

/** Pointer type based on another field type
  *
  * @param pointeeType  Type pointed to by this type.
  */
case class PointerFieldType(pointeeType: FieldType) extends FieldType

/** Function pointer field type
  *
  * @param returnType  Return type of the function or None for void functions
  * @param arguments   List of arguments to the function
  */
case class FunctionPointerFieldType(returnType: Option[FieldType], arguments: List[FieldType] ) extends FieldType

/** Fixed array field type
  *
  * @param dimensions   Dimensions of the array
  * @param elementType  Type of the array elements
  */
case class ArrayFieldType(dimensions: List[Int], elementType: FieldType) extends FieldType

/** Primitive field type
  *
  * @param signed       Sign of an integer type or None otherwise. This is used
  *                     to comment the LLVM definition.
  * @param llvmType     LLVM type of the field
  * @param cppTypeName  Name of the type in C++
  */
case class PrimitiveFieldType(signed: Option[Boolean], llvmType: llvmir.FirstClassType, cppTypeName: String) extends FieldType

/** Alias to another type with a differnet C++ name
  *
  * This is analogous to a typedef in C++
  *
  * @param aliasedType      Underlying type of the alias
  * @param cppTypeName      New name for the type in C++
  * @param needsDefinition  If true a typedef will be automatically generated
  *                         defining the C++ type name. Otherwise an external
  *                         definition is assumed to exist.
  */
class FieldTypeAlias(
  val aliasedType: FieldType,
  val cppTypeName: Option[String],
  val needsDefinition: Boolean
) extends FieldType with Positional
