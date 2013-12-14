package io.llambda.typegen

import io.llambda.llvmir

sealed abstract class FieldType 

case class PointerFieldType(pointeeType : FieldType) extends FieldType
case class FunctionPointerFieldType(returnType : Option[FieldType], arguments : List[FieldType] ) extends FieldType
case class PrimitiveFieldType(signed : Option[Boolean], llvmType : llvmir.FirstClassType, cppTypeName : String) extends FieldType

class FieldTypeAlias(
  val aliasedType : FieldType,
  val cppTypeName : Option[String],
  val needsDefinition : Boolean
) extends FieldType
