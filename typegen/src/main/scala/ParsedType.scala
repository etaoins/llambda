package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class ParsedType extends Positional

case class ParsedPointerType(pointeeType : ParsedType) extends ParsedType {
  override def toString = pointeeType + "*"
}

case class ParsedFunctionPointerType(returnType : Option[ParsedType], arguments : List[ParsedType]) extends ParsedType {
  override def toString = s"""${returnType.getOrElse("void")} (*)(${arguments.mkString(", ")})"""
}

case class ParsedTypeName(name : String) extends ParsedType {
  override def toString = name
}
