package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class ParsedDefinition extends Positional {
  val name : String
}

sealed abstract trait ParsedCellClassDeclarationLike extends ParsedDefinition

case class ParsedCellField(name : String, fieldType : ParsedType, initializer : Option[Long]) extends Positional 

case class ParsedCellClassDeclaration(name : String) extends ParsedCellClassDeclarationLike

sealed abstract class ParsedCellClassDefinition extends ParsedCellClassDeclarationLike {
  val name : String
  val instanceType : CellClass.InstanceType
  val fields : List[ParsedCellField]
  val visibility : CellClass.Visibility
  val parentOption : Option[String]
} 

sealed abstract class ParsedParentedClassDefinition extends ParsedCellClassDefinition {
  val parent : String
  val parentOption = Some(parent)
}

case class ParsedTaggedClassDefinition(
    name : String,
    instanceType : CellClass.InstanceType,
    parent : String,
    fields : List[ParsedCellField],
    visibility : CellClass.Visibility
  ) extends ParsedParentedClassDefinition

case class ParsedVariantClassDefinition(
    name : String,
    parent : String,
    fields : List[ParsedCellField]
  ) extends ParsedParentedClassDefinition {
  val instanceType = CellClass.Variant
  val visibility = CellClass.Internal
}

case class ParsedRootClassDefinition(
    name : String,
    typeTagField : String,
    fields : List[ParsedCellField],
    visibility : CellClass.Visibility
  ) extends ParsedCellClassDefinition {
  val instanceType = CellClass.Abstract
  val parentOption = None
}

case class ParsedCppType(
  name : String,
  needsDefinition : Boolean
)

case class ParsedFieldTypeAlias(name : String, aliasedType : ParsedType, cppType : Option[ParsedCppType]) extends ParsedDefinition
