package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class ParsedDefinition extends Positional {
  val name : String
}

final class ParsedCellField(
  val name : String,
  val fieldType : String
) extends Positional 

final class ParsedCellClass(
  val name : String,
  val instanceType : CellClass.InstanceType,
  val inherits : Option[String],
  val fields : List[ParsedCellField],
  val internal : Boolean
) extends ParsedDefinition

case class ParsedCType(
  name : String,
  externallyDefined : Boolean
)

final class ParsedUserDefinedFieldType(
  val name : String,
  val inherits : String,
  val ctype : Option[ParsedCType]
) extends ParsedDefinition
