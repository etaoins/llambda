package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class ParsedDefinition extends Positional {
  val name : String
}

sealed abstract trait ParsedCellClassDeclarationLike extends ParsedDefinition

case class ParsedCellField(name : String, fieldType : ParsedType)  extends Positional 

case class ParsedCellClassDeclaration(name : String) extends ParsedCellClassDeclarationLike

sealed abstract class ParsedCellClassDefinition extends ParsedCellClassDeclarationLike {
  val name : String
  val instanceType : CellClass.InstanceType
  val fields : List[ParsedCellField]
  val internal : Boolean
  val optionalParent : Option[String]
} 

case class ParsedChildClassDefinition(name : String, instanceType : CellClass.InstanceType, parent : String, fields : List[ParsedCellField], internal : Boolean) extends ParsedCellClassDefinition {
  val optionalParent = Some(parent)
}

case class ParsedRootClassDefinition(name : String, fields : List[ParsedCellField], internal : Boolean) extends ParsedCellClassDefinition {
  val instanceType = CellClass.Abstract
  val optionalParent = None
}

case class ParsedCppType(
  name : String,
  needsDefinition : Boolean
)

case class ParsedFieldTypeAlias(name : String, aliasedType : ParsedType, cppType : Option[ParsedCppType]) extends ParsedDefinition
