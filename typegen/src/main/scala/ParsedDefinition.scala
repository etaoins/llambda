package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class ParsedDefinition extends Positional {
  val name : String
}

sealed abstract trait ParsedCellClassDeclarationLike extends ParsedDefinition

final class ParsedCellField(
  val name : String,
  val fieldType : ParsedType
) extends Positional 

final class ParsedCellClassDeclaration(
  val name : String
) extends ParsedCellClassDeclarationLike

final class ParsedCellClassDefinition(
  val name : String,
  val instanceType : CellClass.InstanceType,
  val inherits : Option[String],
  val fields : List[ParsedCellField],
  val internal : Boolean
) extends ParsedCellClassDeclarationLike

case class ParsedCppType(
  name : String,
  needsDefinition : Boolean
)

final class ParsedUserDefinedFieldType(
  val name : String,
  val aliasOf : ParsedType,
  val cppType : Option[ParsedCppType]
) extends ParsedDefinition
